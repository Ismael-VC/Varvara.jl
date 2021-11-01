module VarvaraOS

using Printf: @sprintf

import Match

using  Match: @match

using ..UxnUtils: bool
using ..Uxn: CPU, Memory, Stack, Device, PAGE_PROGRAM, peek16, poke16, uxn_eval!

export load!, system_talk!, console_talk, file_talk!, datetime_talk!, inspect,
 boot!


#= Generics =#
const FILDES = Dict(
  0 => stdin,
  1 => stdout,
  2 => stderr
)

const EXCEPTIONS = (
  :UxnInputError, :UxnLoadError, :UxnInitError, :UnderflowErro,
  :UxnUnderflowError, :UxnOverflowError, :UxnZeroDivisionError
)

for e in EXCEPTIONS
  @eval struct $e
    msg::String
  end
end

const UXN_ERRORS = [UxnUnderflowError, UxnOverflowError, UxnZeroDivisionError]

function uxn_halt(c::CPU, err::UInt8, name::AbstractString, id::Int)::Exception
  @error "Halted"
  throw(UXN_ERRORS[error](@sprintf("%s#%04x, at 0x%04x", id, c.ram.ptr)))
end

#= Core =#
function console_input!(c::CPU, ch::Char)::Int
  dev_console.dat[0x2] = ch
  return uxn_eval(c, dev_console.vector)
end

function run!(c::CPU)::Nothing
  dev_console = c.devs[1]
  while iszero(c.devs[0].dat[0xf]) && dev_console.dat[0x2] > 0
    vec = peek16(dev_console.dat, 0)
    iszero(vec) && (vec = c.ram.ptr)  # continue after last BRK
    uxn_eval!(c, vec)
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

function load!(c::CPU, filepath::AbstractString)::Bool
  begin
    rom = read(filepath)
    used = PAGE_PROGRAM + length(rom) - 1
    free = 65279  # typemax(UInt16) - PAGE_PROGRAM
    @assert free - used > 0
    @inbounds c.ram.dat[PAGE_PROGRAM:PAGE_PROGRAM + length(rom) - 1] = rom
    @info "Loaded $filepath"

    return true
  end

  @error("Load: Failed")
  return false
end

function boot!()::Int
  loaded = false
  c = CPU()

  #= system   =# dev_system = Device(c, 0x0, system_talk!)
  #= console  =# dev_console = Device(c, 0x1, console_talk)
  #= empty    =# Device(c, 0x2)
  #= empty    =# Device(c, 0x3)
  #= empty    =# Device(c, 0x4)
  #= empty    =# Device(c, 0x5)
  #= empty    =# Device(c, 0x6)
  #= empty    =# Device(c, 0x7)
  #= empty    =# Device(c, 0x8)
  #= empty    =# Device(c, 0x9)
  #= file     =# dev_file = Device(c, 0xa, file_talk!)
  #= datetime =# dev_datetime = Device(c, 0xb, datetime_talk!)
  #= empty    =# Device(c, 0xc)
  #= empty    =# Device(c, 0xd)
  #= empty    =# Device(c, 0xe)
  #= empty    =# Device(c, 0xf)

  for (i, rom) in enumerate(ARGS)
    if !loaded
      !load!(c, rom) && return 0
      loaded = true
      !uxn_eval!(c, PAGE_PROGRAM) && (@error("Init: Failed"); return 0)
    else
      arg = ARGS[i]
      while bool(arg); console_input!(c, arg); arg += 1; end
      console_input!(c, '\n')
    end
  end
  loaded || (@error("Input: Missing"); return 0)

  run!(c)

  return 0
end

#= Devices =#
function system_talk!(d::Device, b0::UInt8, wr::Bool = false)::Bool
  if wr  #= read =#
    @match b0 begin
      0x2 => (d.dat[2] = d.c.wst.ptr)
      0x3 => (d.dat[3] = d.c.rst.ptr)
    end

  else #= write =#
    @match b0 begin
      0x2 => (d.c.wst.ptr = d.dat[2])
      0x3 => (d.c.rst.ptr = d.dat[3])
      0xe => begin
        inspect(d.c.wst, "Working-stack")
        inspect(d.c.rst, "Return-stack")
      end
      0xf => return false
    end
  end

  return true
end

function console_talk(d::Device, b0::UInt8, wr::Bool = false)::Bool
  b0 == 0x1 && (d.vector = peek16(d.dat, 0x0))
  (wr && b0 > 0x7) && write(FILDES[b0 - 0x7], Char(d.dat[b0]))

  return true
end

function file_talk!(d::Device, b0::UInt8, wr::Bool = false)::Bool
  read = b0 == 0xd

  if wr && (read || b0 == 0xf)
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

function datetime_talk!(d::Device, b0::UInt8, wr::Bool = false)::Bool
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
    t = now()
    poke16(d.dat, 0x0, year(t))
    d.dat[0x2] = month(t)
    d.dat[0x3] = dayofmoth(t)
    d.dat[0x4] = hour(t)
    d.dat[0x5] = minute(t)
    d.dat[0x6] = second(t)
    d.dat[0x7] = dayofweek(t)
    poke16(d.dat, 0x08, dayofyear(t))
    d.dat[0xa] = reinterpret(UInt8, Int8(-1))
  end

  return true
end

end  # module

if abspath(PROGRAM_FILE) == @__FILE__
    using .VarvaraOS: boot!

    boot!()
end
