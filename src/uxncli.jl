#=
Copyright (c) 2021 Devine Lu Linvega

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

module UxnCLI

using Dates
using Match
using UXN

export devconsole, devsystem, inspect, system_talk, nil_talk, datetime_talk,
	   console_talk


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
		sp = s.dat[p + 1]
		head *= p == s.ptr ? @sprintf("[%02x] ", sp) : @sprintf("%02x ", sp)
	end
	@info head
end


# Devices

function system_talk(d::Device, b0::UInt8, w::UInt8)::Int
	if iszero(w)  #= read =#
		@match b0 begin
			0x2 => d.dat[2 + 1] = d.u.wst.ptr
			0x3 => d.dat[3 + 1] = d.u.rst.ptr
		end
	else #= write =#
		@match b0 begin
			0x2 => d.u.wst.ptr = d.dat[2 + 1]
			0x3 => d.u.rst.ptr = d.dat[3 + 1]
			0xe => begin
				inspect(d.u.wst, "Working-stack")
				inspect(d.u.rst, "Return-stack")
			end
			0xf => return 0
		end
	end
	return 1
end

function console_talk(d::Device, b0::UInt8, w::UInt8)
	b0 == 0x1 && (d.vector = peek16(d.dat, 0x0))
	fildes = Dict(
		0 => stdin,
		1 => stdout,
		2 => stderr
	)
	(!iszero(w) && b0 > 0x7) && write(fildes[b0 - 0x7], Char(d.dat[b0 + 1]))

	return 1
end


function file_talk(Device d, Uint8 b0, Uint8 w)::Bool
	read = b0 == 0xd
	if(w && (read || b0 == 0xf))
		char name = (char )d.mem[peek16(d.dat, 0x8)]
		Uint16 result = 0, length = peek16(d.dat, 0xa)
		long offset = (peek16(d.dat, 0x4) << 16) + peek16(d.dat, 0x6)
		Uint16 addr = peek16(d.dat, b0 - 1)
		FILE f = fopen(name, read ? "rb" : (offset ? "ab" : "wb"))
		if(f)
			if(fseek(f, offset, SEEK_SET) != -1)
				result = read ? fread(d.mem[addr], 1, length, f) : fwrite(d.mem[addr], 1, length, f)
			fclose(f)
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

function datetime_talk(d::Device, b0::UInt8, w::UInt8)::Int
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
		d.dat[0x2 + 1] = t.month
		d.dat[0x3 + 1] = t.dayofmoth
		d.dat[0x4 + 1] = t.hour
		d.dat[0x5 + 1] = t.minute
		d.dat[0x6 + 1] = t.second
		d.dat[0x7 + 1] = t.dayofweek
		poke16(d.dat, 0x08, t.dayofyear)
		d.dat[0xa + 1] = t.is_dst
	catch
		t = Dates.now()
		poke16(d.dat, 0x0, Dates.year(t))
		d.dat[0x2 + 1] = Dates.month(t)
		d.dat[0x3 + 1] = Dates.dayofmoth(t)
		d.dat[0x4 + 1] = Dates.hour(t)
		d.dat[0x5 + 1] = Dates.minute(t)
		d.dat[0x6 + 1] = Dates.second(t)
		d.dat[0x7 + 1] = Dates.dayofweek(t) + 1
		poke16(d.dat, 0x08, Dates.dayofyear(t))
		d.dat[0xa + 1] = reinterpret(UInt8, Int8(-1))
	end

	return 1
end

nil_talk(d::Device, b0::Uint8, w::Uint8)::Int = return 1

# Generics

const UXN_ERRORS = [UxnUnderflowError, UxnOverflowError, UxnZeroDivisionError]

function uxn_halt(u::Uxn, err::UInt8, name::AbstractString, id::Int)::Exception
	@error "Halted"
	throw UXN_ERRORS[error](@sprintf("%s#%04x, at 0x%04x", id, u.ram.ptr))
end

function console_input(u::Uxn, c::Char)::Int
	devconsole.dat[0x2] = c
	return uxn_eval(u, devconsole.vector)
end


function run!(u::Uxn)::Nothing
	while iszero(u.dev[0].dat[0xf]) && read(0, devconsole.dat[0x2 + 1], 1) > 0
		vec = peek16(devconsole.dat, 0)
		iszero(vec) && (vec = u.ram.ptr)  # continue after last BRK
		uxn_eval(u, vec)
	end
end


function load(u::Uxn, filepath::AbstractString)::Bool
	try
		io = open(filepath)
		read!(io, @view u.ram.dat[PAGE_PROGRAM + 1:end])
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
	u = Uxn()

	#= system   =# devsystem = uxn_port(u, 0x0, system_talk)
	#= console  =# devconsole = uxn_port(u, 0x1, console_talk)
	#= empty    =# uxn_port(u, 0x2, nil_talk)
	#= empty    =# uxn_port(u, 0x3, nil_talk)
	#= empty    =# uxn_port(u, 0x4, nil_talk)
	#= empty    =# uxn_port(u, 0x5, nil_talk)
	#= empty    =# uxn_port(u, 0x6, nil_talk)
	#= empty    =# uxn_port(u, 0x7, nil_talk)
	#= empty    =# uxn_port(u, 0x8, nil_talk)
	#= empty    =# uxn_port(u, 0x9, nil_talk)
	#= file     =# uxn_port(u, 0xa, file_talk)
	#= datetime =# uxn_port(u, 0xb, datetime_talk)
	#= empty    =# uxn_port(u, 0xc, nil_talk)
	#= empty    =# uxn_port(u, 0xd, nil_talk)
	#= empty    =# uxn_port(u, 0xe, nil_talk)
	#= empty    =# uxn_port(u, 0xf, nil_talk)

	for rom in ARGS
		if !loaded
			!load(u, rom) && (; return 0)
			!uxn_eval(u, PAGE_PROGRAM))
				(@error("Init: Failed"); return 0)
		else
			char p = argv[i]
			while(p) console_input(u, p++)
			console_input(u, '\n')
		end
	end
	loaded || (@error("Input: Missing"); return 0)

	run!(u)

	return 0
end

end  # module
