#!/usr/bin/env julia

#=
Copyright (u) 2021 Devine Lu Linvega
Copyright (u) 2021 Andrew Alderwick
Copyright (u) 2021 Ismael Venegas Castelló

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

module UXN

using Match  #: @match
using OffsetArrays: OffsetVector
using StaticArrays: SizedVector

export Uxn, AbstractUxn, Stack, Memory, Device, uxn_halt, uxn_port, uxn_eval,

	     push8,  pop8k, pop8d,  poke8,  peek8,  devw8,  devr8,  warp,  pull8,
       push16, pop8d, pop16d, poke16, peek16, devw16, devr16,        pull16,
	             pop16,

       low_byte, high_byte, boot!,   bool, set!,
                            reset!,
                            reboot!,

       PAGE_PROGRAM, MODE_SHORT, MODE_RETURN, MODE_KEEP

const PAGE_PROGRAM = 0x0100
const MODE_SHORT = 0x20
const MODE_RETURN = 0x40
const MODE_KEEP = 0x80


const StackData = OffsetVector{UInt8, SizedVector{255, UInt8, Vector{UInt8}}}

function OffsetSizedVector(type::Type, offset::AbstractRange, lenght::Integer, data::AbstractVector{T}) where {T}
  return OffsetVector(SizedVector{lenght, type, Vector{type}}(data), offset)
end

mutable struct Stack
  ptr::UInt8
  kptr::UInt8
  error::UInt8
  dat::StackData

  Stack() = new(0, 0, 0, OffsetSizedVector(UInt8, 0:254, 255, zeros(UInt8, 255)))
end

reset!(s::Stack) = (s.ptr = s.kptr = s.error = 0; fill!(s.dat, 0); s)
set!(s::Stack, data::Vector{UInt8})::Nothing = (s.dat = OffsetSizedVector(UInt8, 0:254, 255, data); return)

function set!(s::Stack, field::Symbol, data::UInt8)::Nothing
  valid_fields = (:ptr, :kptr, :error)
  if field in valid_fields
    setfield!(s, field, data)
  else
    throw(ArgumentError("$field not in $valid_fields"))
  end
  return nothing
end


const MemoryData = OffsetVector{UInt8, SizedVector{65535, UInt8, Vector{UInt8}}}

mutable struct Memory
  ptr::UInt16
  dat::MemoryData

  Memory() = new(0, OffsetSizedVector(UInt8, 0:65534, 65535, zeros(UInt8, 65535)))
end

reset!(m::Memory) = (m.ptr = 0; fill!(m.dat, 0); m)
set!(m::Memory, ptr::UInt16)::Nothing = (m.ptr = ptr; return)
set!(m::Memory, data::Vector{UInt8})::Nothing = (m.dat = OffsetSizedVector(UInt8, 0:65534, 65535, data); return)



abstract type AbstractUxn end

const DeviceData = OffsetVector{UInt8, SizedVector{16, UInt8, Vector{UInt8}}}

mutable struct Device{U <: AbstractUxn}
  u::U
  addr::UInt8
  vector::UInt16
  talk::Union{Function, Nothing}
  mem::MemoryData
  dat::DeviceData

  function Device(u::U) where {U <: AbstractUxn}
    new{U}(u, 0, 0, nothing, u.ram.dat, OffsetSizedVector(UInt8, 0:15, 16, zeros(UInt8, 16)))
  end
end

reset!(d::Device) = (d.addr = d.mem = vector = 0; fill!(d.dat, 0); d)
set!(d::Device, data::Vector{UInt8})::Nothing = (d.dat = OffsetSizedVector(UInt8, 0:15, 16, data); return)

function set!(d::Device, field::Symbol, data::Union{UInt8, UInt16})::Nothing
  valid_fields = (:addr, :vector)
  if field in valid_fields
    d_field = getfield(d, field)
    if isa(data, typeof(d_field))
      setfield!(d, field, data)
    else
      throw(ArgumentError("$field is not of type $(typeof(d_field))"))
    end
  else
    throw(ArgumentError("$field not in $valid_fields"))
  end
  return nothing
end

const DeviceVector = OffsetVector{Device, SizedVector{16, Device, Vector{Device}}}

mutable struct Uxn <: AbstractUxn
  wst::Stack
  rst::Stack
  src::Stack
  dst::Stack
  ram::Memory
  dev::DeviceVector

  function Uxn()

    this = new(Stack(), Stack(), Stack(), Stack(), Memory(),
              OffsetVector(SizedVector{16, Device, Vector{Device}}(undef), 0:15))

    fill!(this.dev, Device(this))
    return this
  end
end

reset!(u::Uxn) = (foreach([u.wst, u.rst, u.src, u.dst, u.ram, u.dev...], reset!); u)
boot! = reboot! = reset!

function set!(u::Uxn, field::Symbol, data::Vector{UInt8})::Nothing
  valid_fields = (:wst, :rst, :src, :dst, :ram)
  if field in valid_fields
    u_field = getfield(d, field)
    set!(u_field, data)
  else
    throw(ArgumentError("$field not in $valid_fields"))
  end
  return nothing
end


low_byte(x::UInt16)::UInt8 = x & 0xff
high_byte(x::UInt16)::UInt8 = x >> 8
bool(x::Number)::Bool = !iszero(x)
bool(x::Union{AbstractArray, AbstractString})::Bool = !isempty(x)


#= byte mode =#
push8(s::Stack, a::UInt8)::Nothing = s.ptr == 0xff ? (s.error = 2; return) : (s.dat[s.ptr] = a; s.ptr += 1; return)
pop8k(s::Stack)::UInt8 = s.kptr == 0 ? (s.error = 1; return 0) : (s.kptr -= 1; return s.dat[s.kptr])
pop8d(s::Stack)::UInt8 = s.ptr == 0 ? (s.error = 1; return 0) : (s.ptr -= 1; return s.dat[s.ptr])
poke8(m::AbstractVector{UInt8}, a::Integer, b::UInt8)::Nothing = (m[a] = b; return)
peek8(m::AbstractVector{UInt8}, a::Integer)::UInt8 = m[a]
devw8(d::Device, a::UInt8, b::UInt8)::UInt8 = (d.dat[a & 0xf] = b; d.talk(d, a & 0x0f, 1))
devr8(d::Device, a::UInt8)::UInt8 = (d.talk(d, a & 0x0f, 0); return d.dat[a & 0xf])
warp(u::Uxn, a::Integer)::Nothing = (u.ram.ptr += a; return)
pull8(u::Uxn)::Nothing = (push8(u.src, peek8(u.ram.dat, u.ram.ptr)); u.ram.ptr += 1; return)

#= short mode =#
push16(s::Stack, a::UInt16)::Nothing = (push8(s, high_byte(a)); push8(s, low_byte(a)))
pop16(s::Stack)::UInt16 = ((a = pop8(s), b = pop8(s)); a + b << 8)
poke16(m::AbstractVector{UInt8}, a::Integer, b::UInt16)::Nothing = (poke8(m, a, b >> 8); poke8(m, a + 1, b))
peek16(m::AbstractVector{UInt8}, a::Integer)::UInt16 = (peek8(m, a) << 8) + peek8(m, a + 1)
devw16(d::Device, a::UInt8, b::UInt16)::Int = devw8(d, a, b >> 8) && devw8(d, a + 1, b)
devr16(d::Device, a::UInt8)::UInt16 = (devr8(d, a) << 8) + devr8(d, a + 1)
pull16(u::Uxn)::Nothing = (push16(u.src, peek16(u.ram.dat, u.ram.ptr)); u.ram.ptr += 2; nothing)


# Core
function uxn_eval(u::Uxn, vec::UInt16, fault_handler::Function)::Int
  (!bool(vec) || bool(u.dev[0].dat[0xf])) && return 0
  u.ram.ptr = vec
  u.wst.ptr > 0xf8 && (u.wst.ptr = 0xf8)

  @show u.ram.dat[u.ram.ptr]
  while (instr = u.ram.dat[u.ram.ptr]; u.ram.ptr += 1; bool(instr))
    #= Return Mode =#
    if bool(instr & MODE_RETURN)
      u.src = u.rst
      u.dst = u.wst
    else
      u.src = u.wst
      u.dst = u.rst
    end

    #= Keep Mode =#
    if bool(instr & MODE_KEEP)
      pop8 = pop8k
      u.src.kptr = u.src.ptr
    else
      pop8 = pop8d
    end

    #= Short Mode =#
    if bool(instr & MODE_SHORT)
      push = push16; pop = pop16
      poke = poke16; peek = peek16
      devw = devw16; devr = devr16
      pull = pull16
    else
      push = push8; pop = pop8
      poke = poke8; peek = peek8
      devw = devw8; devr = devr8
      pull = pull8
    end

    @show (instr & 0x1f)

    @match (instr & 0x1f) begin
      #= Stack =#
      0x00 => #= LIT =# pull(u)
      0x01 => #= INC =# (a = pop(u.src); push(u.src, a + 0x1))
      0x02 => #= POP =# pop(u.src)
      0x03 => #= DUP =# (a = pop(u.src); push(u.src, a); push(u.src, a))
      0x04 => #= NIP =# (a = pop(u.src); pop(u.src); push(u.src, a))
      0x05 => #= SWP =# (a = pop(u.src); b = pop(u.src); push(u.src, a); push(u.src, b))
      0x06 => #= OVR =# (a = pop(u.src); b = pop(u.src); push(u.src, b); push(u.src, a); push(u.src, b))
      0x07 => #= ROT =# (a = pop(u.src); b = pop(u.src); c = pop(u.src); push(u.src, b); push(u.src, a); push(u.src, c))

      #= Logic =#
      0x08 => #= EQU =# (a = pop(u.src); b = pop(u.src); push8(u.src, b == a))
      0x09 => #= NEQ =# (a = pop(u.src); b = pop(u.src); push8(u.src, b != a))
      0x0a => #= GTH =# (a = pop(u.src); b = pop(u.src); push8(u.src, b > a))
      0x0b => #= LTH =# (a = pop(u.src); b = pop(u.src); push8(u.src, b < a))
      0x0c => #= JMP =# (a = pop(u.src); warp(u, a))
      0x0d => #= JCN =# (a = pop(u.src); (pop8(u.src) && warp(u, a)))
      0x0e => #= JSR =# (a = pop(u.src); push16(u.dst, u.ram.ptr); warp(u, a))
      0x0f => #= STH =# (a = pop(u.src); push(u.dst, a))

      #= Memory =#
      0x10 => #= LDZ =# (a = pop8(u.src); push(u.src, peek(u.ram.dat, a)))
      0x11 => #= STZ =# (a = pop8(u.src); b = pop(u.src); poke(u.ram.dat, a, b))
      0x12 => #= LDR =# (a = pop8(u.src); push(u.src, peek(u.ram.dat, u.ram.ptr + Int8(a))))
      0x13 => #= STR =# (a = pop8(u.src); b = pop(u.src); poke(u.ram.dat, u.ram.ptr + Int8(a), b))
      0x14 => #= LDA =# (a = pop16(u.src); push(u.src, peek(u.ram.dat, a)))
      0x15 => #= STA =# (a = pop16(u.src); b = pop(u.src); poke(u.ram.dat, a, b))
      0x16 => #= DEI =# (a = pop8(u.src); push(u.src, devr(u.dev[a >> 4], a)))
      0x17 => #= DEO =# (a = pop8(u.src); b = pop(u.src); (!devw(u.dev[a >> 4], a, b) && return 1))

      #= Arithmetic =#
      0x18 => #= ADD =# (a = pop(u.src); b = pop(u.src); push(u.src, b + a))
      0x19 => #= SUB =# (a = pop(u.src); b = pop(u.src); push(u.src, b - a))
      0x1a => #= MUL =# (a = pop(u.src); b = pop(u.src); push(u.src, b * a))
      0x1b => #= DIV =# (a = pop(u.src); b = pop(u.src); (a == 0 && (u.src.error = 3; a = 1)); push(u.src, b / a))
      0x1c => #= AND =# (a = pop(u.src); b = pop(u.src); push(u.src, b & a))
      0x1d => #= ORA =# (a = pop(u.src); b = pop(u.src); push(u.src, b | a))
      0x1e => #= EOR =# (a = pop(u.src); b = pop(u.src); push(u.src, b ^ a))
      0x1f => #= SFT =# (a = pop8!(u.src); b = pop(u.src); push(u.src, b >> (a & 0x0f) << ((a & 0xf0) >> 4)))
    end

    bool(u.wst.error) && return fault_handler(u, u.wst.error, "Working-stack", instr)
    bool(u.rst.error) && return fault_handler(u, u.rst.error, "Return-stack", instr)
  end

  return 1
end

uxn_eval(fault_handler::Function, u::Uxn, vec::UInt16) = uxn_eval(u, vec, fault_handler)


function attach_device(u::Uxn, id::Int, talkfn::Function)::Device
  d = u.dev[id]
  d.addr = id * 0x10
  d.u = u
  d.mem = u.ram.dat
  d.talk = talkfn

  return d
end

attach_device(talkfn::Function, u::Uxn, id::Int) = attach_device(u, id, talkfn)


end  # module
