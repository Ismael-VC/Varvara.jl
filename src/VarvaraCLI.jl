#=
Copyright (c) 2021 Devine Lu Linvega
Copyright (c) 2021 Ismael Venegas Castelló

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

module VarvaraCLI

using Dates
using Printf: @sprintf

import Match
using Match: @match

using VarvaraEmulator

export dev_console, dev_system, dev_datetime, dev_file,
       inspect, system_talk, nil_talk, datetime_talk,
       console_talk


const FILDES = Dict(
  0 => stdin,
  1 => stdout,
  2 => stderr
)


# Core

const EXCEPTIONS = (
  :UxnInputError, :UxnLoadError, :UxnInitError, :UnderflowErro,
  :UxnUnderflowError, :UxnOverflowError, :UxnZeroDivisionError
)

for e in EXCEPTIONS
  @eval struct $e
    msg::String
  end
end

function inspect(s::Stack, name::AbstractString)::Nothing
  @info name
  head = ""
  for y in 0:3, x in 0:7
    p = y * 8 + x
    sp = s.dat[p]
    head *= p == s.ptr ? @sprintf("[%02x] ", sp) : @sprintf("%02x ", sp)
  end
  @info head
end


# Devices

function system_talk(d::Device, b0::UInt8, w::UInt8)::Bool
  if w  #= read =#
    @match b0 begin
      0x2 => (d.dat[2] = d.u.wst.ptr)
      0x3 => (d.dat[3] = d.u.rst.ptr)
    end
  else #= write =#
    @match b0 begin
      0x2 => (d.u.wst.ptr = d.dat[2])
      0x3 => (d.u.rst.ptr = d.dat[3])
      0xe => begin
        inspect(d.u.wst, "Working-stack")
        inspect(d.u.rst, "Return-stack")
      end
      0xf => return false
    end
  end
  return true
end

function console_talk(d::Device, b0::UInt8, w::UInt8)::Bool
  b0 == 0x1 && (d.vector = peek16(d.dat, 0x0))
  (!iszero(w) && b0 > 0x7) && write(FILDES[b0 - 0x7], Char(d.dat[b0]))

  return true
end


function file_talk(d::Device, b0::UInt8, w::Bool)::Bool
  read = b0 == 0xd
  if w && (read || b0 == 0xf)
    name::Char = Char(d.mem[peek16(d.dat, 0x8)])
    result::UInt16 = 0
    length::UInt16 = peek16(d.dat, 0xa)
    offset::Int32 = (Int32(peek16(d.dat, 0x4) << 16)) + peek16(d.dat, 0x6)
    addr::UInt16 = peek16(d.dat, b0 - 1)
    open(name, read ? "r" : (offset ? "a" : "w")) do f::IOStream
      fseek(f, offset)
      result = read ? read(d.mem[addr], 1, length, f) : write(d.mem[addr], 1, length, f)
    end
    poke16(d.dat, 0x2, result)
  end
  return true
end

struct Ctm
  second::Cint
  minute::Cint
  hour::Cint
  dayofmoth::Cint
  month::Cint
  year::Cint
  dayofweek::Cint
  dayofyear::Cint
  is_dst::Cint
end

function datetime_talk(d::Device, b0::UInt8, w::UInt8)::Bool
  result = Ref{Int64}(0)

  try
    localtime = ccall(
      @static(Sys.iswindows() ? :localtime : (:localtime, "libc.so.6")),
      Ptr{Int64},
      (Ptr{Int64},),
      result
    )

    t = unsafe_load(localtime)
    t.year += 1900

    poke16(d.dat, 0x0, t.year)
    d.dat[0x2] = t.month
    d.dat[0x3] = t.dayofmoth
    d.dat[0x4] = t.hour
    d.dat[0x5] = t.minute
    d.dat[0x6] = t.second
    d.dat[0x7] = t.dayofweek
    poke16(d.dat, 0x08, t.dayofyear)
    d.dat[0xa] = t.is_dst
  catch
    t = Dates.now()
    poke16(d.dat, 0x0, Dates.year(t))
    d.dat[0x2] = Dates.month(t)
    d.dat[0x3] = Dates.dayofmoth(t)
    d.dat[0x4] = Dates.hour(t)
    d.dat[0x5] = Dates.minute(t)
    d.dat[0x6] = Dates.second(t)
    d.dat[0x7] = Dates.dayofweek(t)
    poke16(d.dat, 0x08, Dates.dayofyear(t))
    d.dat[0xa] = reinterpret(UInt8, Int8(-1))
  end

  return true
end



# Generics

const UXN_ERRORS = [UxnUnderflowError, UxnOverflowError, UxnZeroDivisionError]

function uxn_halt(u::UxnCPU, err::UInt8, name::AbstractString, id::Int)::Exception
  @error "Halted"
  throw(UXN_ERRORS[error](@sprintf("%s#%04x, at 0x%04x", id, u.ram.ptr)))
end

function console_input(u::UxnCPU, c::Char)::Int
  dev_console.dat[0x2] = c
  return uxn_eval(u, dev_console.vector)
end


function run!(u::UxnCPU)::Nothing
  while iszero(u.dev[0].dat[0xf]) && read(0, dev_console.dat[0x2], 1) > 0
    vec = peek16(dev_console.dat, 0)
    iszero(vec) && (vec = u.ram.ptr)  # continue after last BRK
    uxn_eval(u, vec)
  end
end


function load(u::UxnCPU, filepath::AbstractString)::Bool
  try
    io = open(filepath)
    read!(io, @view u.ram.dat[PAGE_PROGRAM:end])
    @info "Loaded $filepath"
      return true
  catch
    @error("Load: Failed")
    return false
  finally
    close(io)
  end
end


function main()::Int
  loaded = false
  u = UxnCPU()

  #= system   =# dev_system = Device(u, 0x0, system_talk)
  #= console  =# dev_console = Device(u, 0x1, console_talk)
  #= empty    =# Device(u, 0x2)
  #= empty    =# Device(u, 0x3)
  #= empty    =# Device(u, 0x4)
  #= empty    =# Device(u, 0x5)
  #= empty    =# Device(u, 0x6)
  #= empty    =# Device(u, 0x7)
  #= empty    =# Device(u, 0x8)
  #= empty    =# Device(u, 0x9)
  #= file     =# dev_file = Device(u, 0xa, file_talk)
  #= datetime =# dev_datetime = Device(u, 0xb, datetime_talk)
  #= empty    =# Device(u, 0xc)
  #= empty    =# Device(u, 0xd)
  #= empty    =# Device(u, 0xe)
  #= empty    =# Device(u, 0xf)

  for rom in ARGS
    if !loaded
      !load(u, rom) && return 0
      !uxn_eval(u, PAGE_PROGRAM)
        (@error("Init: Failed"); return 0)
    else
      arg = argv[i]
      while bool(p); console_input(u, p); p += 1; end
      console_input(u, '\n')
    end
  end
  loaded || (@error("Input: Missing"); return 0)

  run!(u)

  return 0
end


end  # module


if abspath(PROGRAM_FILE) == @__FILE__
    using .VarvaraCLI: main

    main()
end
