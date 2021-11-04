#=
  Copyright (c) 2021
  https://github.com/Ismael-VC/Varvara.jl/blob/main/CONTRIBUTORS.md

  Permission to use, copy, modify, and distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE.
=#

module Uxn


import Match
import UUIDs

using Match: @match
using OffsetArrays: OffsetVector
using Printf: @sprintf
using UUIDs: UUID, uuid4

using ..UxnUtils: high_byte, low_byte, concat_bytes, bool

export  CPU, AbstractCPU,  Stack,  Memory,      Device,    uxn_eval, OVector,
  Void, PAGE_PROGRAM, MODE_SHORT,  MODE_RETURN, MODE_KEEP, uxn_boot,

  push8,  pop8k, pop8d,  poke8,  peek8,  devw8,  devr8,  pull8,  warp,
  push16, pop8d, pop16d, poke16, peek16, devw16, devr16, pull16, pop16,

  LIT, INC, POP, DUP, NIP, SWP, OVR, ROT, EQU, NEQ, GTH, LTH, JMP, JCN, JSR, STH,
  LDZ, STZ, LDR, STR, LDA, STA, DEI, DEO, ADD, SUB, MUL, DIV, AND, ORA, EOR, SFT

const PAGE_PROGRAM = 0x0100
const MODE_SHORT = 0x20
const MODE_RETURN = 0x40
const MODE_KEEP = 0x80
const Void = Nothing
const OVector{T} = OffsetVector{T, Vector{T}} where {T}

OVector{T}(len::Integer) where {T} = OffsetVector(Vector{T}(undef, len), 0:len-1)
OVector(T::DataType, len::Integer) = OffsetVector(zeros(T, len), 0:len-1)


mutable struct Stack
  ptr::UInt8
  kptr::UInt8
  error::UInt8
  dat::OVector{UInt8}
end

mutable struct Memory
  ptr::UInt16
  dat::OVector{UInt8}
end

abstract type AbstractCPU end;

mutable struct Device{C<:AbstractCPU, F<:Function}
  c::C
  id::UInt8
  vec::UInt16
  talk::F
  mem::OVector{UInt8}
  dat::OVector{UInt8}
end

mutable struct CPU <: AbstractCPU
  uuid::UUID
  wst::Stack
  rst::Stack
  src::Stack
  dst::Stack
  ram::Memory
  dev::OVector{Device}
end


Stack()::Stack  = Stack(0, 0, 0, OVector(UInt8, 0xff))
Memory()::Memory = Memory(0, OVector(UInt16, 0xffff))
CPU()::CPU = CPU(uuid4(), Stack(), Stack(), Stack(), Stack(), Memory(), OVector{Device}(0x10))
(Device(talkfn::F, c::C, id::UInt8)::Device) where {C<:AbstractCPU, F<:Function} = Device(c, id, talkfn)

function Device(c::C, id::UInt8, talkfn::F)::Device where {C<:AbstractCPU, F<:Function}
  d = Device(c, id, 0x000, talkfn, c.ram.dat, OVector{UInt8}(0x10))
  c.dev[id] = d

  return d
end

function Device(c::C, id::UInt8)::Device where {C<:AbstractCPU}
  Device(c, id) do d::Device, b0::UInt8, w::UInt8
    return true
  end
end

#= Operations =#

#= Byte mode =#
push8(s::Stack, a::UInt8)::Void = s.ptr == 0xff ? (s.error = 2; return) : (s.dat[s.ptr] = a; s.ptr += 1; return)
pop8k(s::Stack)::UInt8 = s.kptr == 0x0 ? (s.error = 1; return 0x0) : (s.kptr -= 1; return s.dat[s.kptr])
pop8d(s::Stack)::UInt8 = s.ptr == 0x0 ? (s.error = 1; return 0x0) : (s.ptr -= 1; return s.dat[s.ptr])
poke8(m::AbstractVector{UInt8}, a::Integer, b::UInt8)::Void = (m[a] = b; return)
peek8(m::AbstractVector{UInt8}, a::Integer)::UInt8 = m[a]
devw8(d::Device, a::UInt8, b::UInt8)::Int = (d.dat[a & 0xf] = b; d.talk(d, a & 0x0f, 0x1))
devr8(d::Device, a::UInt8)::UInt8 = (d.talk(d, a & 0x0f, 0x0); return d.dat[a & 0xf])
warp(c::CPU, a::Integer)::Void = (c.ram.ptr += a; return)
pull8(c::CPU)::Void = (push8(c.src, peek8(c.ram.dat, c.ram.ptr)); c.ram.ptr += 1; return)

#= Short mode =#
push16(s::Stack, a::UInt16)::Void = (push8(s, high_byte(a)); push8(s, low_byte(a)))
pop16(s::Stack)::UInt16 = ((a = pop8(s), b = pop8(s)); a + b << 8)
poke16(m::AbstractVector{UInt8}, a::Integer, b::UInt16)::Void = (poke8(m, a, b >> 8); poke8(m, a + 0x1, b))
peek16(m::AbstractVector{UInt8}, a::Integer)::UInt16 = (peek8(m, a) << 8) + peek8(m, a + 0x1)
devw16(d::Device, a::UInt8, b::UInt16)::Bool = devw8(d, a, b >> 8) && devw8(d, a + 0x1, b)
devr16(d::Device, a::UInt8)::UInt16 = (devr8(d, a) << 8) + devr8(d, a + 0x1)
pull16(c::CPU)::Void = (push16(c.src, peek16(c.ram.dat, c.ram.ptr)); c.ram.ptr += 2; return)


#= Core =#
function uxn_eval(c::CPU, vec::UInt16, uxn_halt!::Function)::Int
  (!bool(vec) || bool(c.dev[0].dat[0xf])) && return 0
  c.ram.ptr = vec
  c.wst.ptr > 0xf8 && (c.wst.ptr = 0xf8)

  while (instr = c.ram.dat[c.ram.ptr]; c.ram.ptr += 1; bool(instr))
    #= Return mode =#
    bool(instr & MODE_RETURN) ? (c.src = c.rst; c.dst = c.wst) :
                                (c.src = c.wst; c.dst = c.rst)

    #= Keep mode =#
    pop8 = bool(instr & MODE_KEEP) ? (c.src.kptr = c.src.ptr; pop8k) : pop8d

    #= Short mode =#
    if bool(instr & MODE_SHORT)
      push = push16; pop = pop16
      poke = poke16; peek = peek16
      devw = devw16; devr = devr16; pull = pull16

    else
      push = push8; pop = pop8
      poke = poke8; peek = peek8
      devw = devw8; devr = devr8; pull = pull8
    end

    @match (instr & 0x1f) begin
      #= Stack =#
      0x00 => #= LIT =# pull(c)
      0x01 => #= INC =# (a = pop(c.src); push(c.src, a + 0x1))
      0x02 => #= POP =# pop(c.src)
      0x03 => #= DUP =# (a = pop(c.src); push(c.src, a); push(c.src, a))
      0x04 => #= NIP =# (a = pop(c.src); pop(c.src); push(c.src, a))
      0x05 => #= SWP =# (a = pop(c.src); b = pop(c.src); push(c.src, a); push(c.src, b))
      0x06 => #= OVR =# (a = pop(c.src); b = pop(c.src); push(c.src, b); push(c.src, a); push(c.src, b))
      0x07 => #= ROT =# (a = pop(c.src); b = pop(c.src); c = pop(c.src); push(c.src, b); push(c.src, a); push(c.src, c))

      #= Logic =#
      0x08 => #= EQU =# (a = pop(c.src); b = pop(c.src); push8(c.src, b == a))
      0x09 => #= NEQ =# (a = pop(c.src); b = pop(c.src); push8(c.src, b != a))
      0x0a => #= GTH =# (a = pop(c.src); b = pop(c.src); push8(c.src, b > a))
      0x0b => #= LTH =# (a = pop(c.src); b = pop(c.src); push8(c.src, b < a))
      0x0c => #= JMP =# (a = pop(c.src); warp(c, a))
      0x0d => #= JCN =# (a = pop(c.src); (pop8(c.src) && warp(c, a)))
      0x0e => #= JSR =# (a = pop(c.src); push16(c.dst, c.ram.ptr); warp(c, a))
      0x0f => #= STH =# (a = pop(c.src); push(c.dst, a))

      #= Memory =#
      0x10 => #= LDZ =# (a = pop8(c.src); push(c.src, peek(c.ram.dat, a)))
      0x11 => #= STZ =# (a = pop8(c.src); b = pop(c.src); poke(c.ram.dat, a, b))
      0x12 => #= LDR =# (a = pop8(c.src); push(c.src, peek(c.ram.dat, c.ram.ptr + Int8(a))))
      0x13 => #= STR =# (a = pop8(c.src); b = pop(c.src); poke(c.ram.dat, c.ram.ptr + Int8(a), b))
      0x14 => #= LDA =# (a = pop16(c.src); push(c.src, peek(c.ram.dat, a)))
      0x15 => #= STA =# (a = pop16(c.src); b = pop(c.src); poke(c.ram.dat, a, b))
      0x16 => #= DEI =# (a = pop8(c.src); push(c.src, devr(c.dev[a >> 4], a)))
      0x17 => #= DEO =# (a = pop8(c.src); b = pop(c.src); (!bool(devw(c.dev[a >> 4], a, b)) && return 1))

      #= Arithmetic =#
      0x18 => #= ADD =# (a = pop(c.src); b = pop(c.src); push(c.src, b + a))
      0x19 => #= SUB =# (a = pop(c.src); b = pop(c.src); push(c.src, b - a))
      0x1a => #= MUL =# (a = pop(c.src); b = pop(c.src); push(c.src, b * a))
      0x1b => #= DIV =# (a = pop(c.src); b = pop(c.src); (a == 0 && (c.src.error = 3; a = 1)); push(c.src, b / a))
      0x1c => #= AND =# (a = pop(c.src); b = pop(c.src); push(c.src, b & a))
      0x1d => #= ORA =# (a = pop(c.src); b = pop(c.src); push(c.src, b | a))
      0x1e => #= EOR =# (a = pop(c.src); b = pop(c.src); push(c.src, b ^ a))
      0x1f => #= SFT =# (a = pop8(c.src); b = pop(c.src); push(c.src, b >> (a & 0x0f) << ((a & 0xf0) >> 4)))
    end

    bool(c.wst.error) && return uxn_halt!(c, c.wst.error, "Working-stack", instr)
    bool(c.rst.error) && return uxn_halt!(c, c.rst.error, "Return-stack", instr)
  end

  return 1
end

uxn_eval(uxn_halt!::Function, c::CPU, vec::UInt16)::Int = uxn_eval(c, vec, uxn_halt!)
uxn_eval(c::CPU, vec::UInt16)::Int = begin
  uxn_eval(c, vec) do c, error, message, instr

    @error "number: $error; message: $message; instruction: $(@sprintf("0x%04x", instr))"
  end
end

reset!(s::Stack) ::Stack  = (s.ptr = s.kptr = s.error = 0x0; fill!(s.dat, 0x0); s)
reset!(m::Memory)::Memory = (m.ptr = 0x0; fill!(m.dat, 0x0); m)
reset!(d::Device)::Device = (vec = 0x0; fill!(d.dat, 0x0); d)
reset!(c::CPU)::CPU = (reset!.([c.wst, c.rst, c.src, c.dst, c.ram, c.dev...]); c)


end  # module
