#=
Copyright (c) 2021 Devine Lu Linvega, Andrew Alderwick, Ismael Venegas Castelló

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

module Uxn

import Match

using        Match: @match
using OffsetArrays: OffsetVector

using ..Utils

export  CPU,  AbstractCPU,  Stack,  Memory,  Device,    uxn_eval!, OVector,
  Void, PAGE_PROGRAM, MODE_SHORT,  MODE_RETURN, MODE_KEEP, reboot!,   bool,

  push8,  pop8k, pop8d,  poke8,  peek8,  devw8,  devr8,  pull8,  warp,
  push16, pop8d, pop16d, poke16, peek16, devw16, devr16, pull16, pop16,

  LIT, INC, POP, DUP, NIP, SWP, OVR, ROT, EQU, NEQ, GTH, LTH, JMP, JCN, JSR, STH,
  LDZ, STZ, LDR, STR, LDA, STA, DEI, DEO, ADD, SUB, MUL, DIV, AND, ORA, EOR, SFT

const PAGE_PROGRAM = 0x0100
const MODE_SHORT   = 0x20
const MODE_RETURN  = 0x40
const MODE_KEEP    = 0x80
const Void         = Nothing
const OVector{T}   = OffsetVector{T, Vector{T}} where {T}

OVector{T}(len::Integer) where {T} = OffsetVector(Vector{T}(undef, len), 0:len-1)
OVector(T::DataType, len::Integer) = OffsetVector(zeros(T, len), 0:len-1)

mutable struct Stack
    ptr::UInt8; kptr::UInt8;
  error::UInt8;  dat::OVector{UInt8}
end

mutable struct Memory
  ptr::UInt16; dat::OVector{UInt8}
end

abstract type AbstractCPU end;
mutable struct Device{C<:AbstractCPU, F<:Function}
     c::C;  id::Int           ; vector::UInt16;
  talk::F; mem::OVector{UInt8};    dat::OVector{UInt8}
end

mutable struct CPU <: AbstractCPU
  wst::Stack; rst::Stack;  src::Stack;
  dst::Stack; ram::Memory; devs::Union{OVector{Device}, Void}
end

 Stack()::Stack  = Stack(0, 0, 0, OVector(UInt8, 0xff))
Memory()::Memory = Memory(0, OVector(UInt16, 0xffff))
CPU()::CPU = CPU(Stack(), Stack(), Stack(), Stack(), Memory(), OVector{Device}(0x10))

(Device(talkfn::F, c::C, id::Int)::Device) where {C<:AbstractCPU, F<:Function} = Device(c, id, talkfn)

function Device(c::C, id::Int, talkfn::F)::Device where {C<:AbstractCPU, F<:Function}
  d = Device(c, id, 0x000, talkfn, c.ram.dat, OVector{UInt8}(0x10))
  c.devs[id] = d

  return d
end

function Device(c::C, id::Int)::Device where {C<:AbstractCPU}
  Device(c, id) do d::Device, b0::UInt8, w::UInt8
    return true
  end
end

#= Byte mode =#
push8(s::Stack, a::UInt8)::Void = s.ptr == 0xff ? (s.error = 2; return) : (s.dat[s.ptr] = a; s.ptr += 1; return)
pop8k(s::Stack)::UInt8 = s.kptr == 0x0 ? (s.error = 0x1; return 0x0) : (s.kptr -= 1; return s.dat[s.kptr])
pop8d(s::Stack)::UInt8 = s.ptr == 0x0 ? (s.error = 0x1; return 0x0) : (s.ptr -= 1; return s.dat[s.ptr])
poke8(m::AbstractVector{UInt8}, a::Integer, b::UInt8)::Void = (m[a] = b; return)
peek8(m::AbstractVector{UInt8}, a::Integer)::UInt8 = m[a]
devw8(d::Device, a::UInt8, b::UInt8)::Bool = (d.dat[a & 0xf] = b; d.talk(d, a & 0x0f, true))
devr8(d::Device, a::UInt8)::UInt8 = (d.talk(d, a & 0x0f, false); return d.dat[a & 0xf])
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

#= Stack =#
LIT(c::CPU)::Void = pull(u)
INC(c::CPU)::Void = (a = pop(c.src); push(c.src, a + 0x1))
POP(c::CPU) = pop(c.src)
DUP(c::CPU)::Void = (a = pop(c.src); push(c.src, a); push(c.src, a))
NIP(c::CPU)::Void = (a = pop(c.src); pop(c.src); push(c.src, a))
SWP(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, a); push(c.src, b))
OVR(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, b); push(c.src, a); push(c.src, b))
ROT(c::CPU)::Void = begin
  a = pop(c.src); b = pop(c.src); c = pop(c.src); push(c.src, b); push(c.src, a); push(c.src, c)
end

#= Logic =#
EQU(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push8(c.src, b == a))
NEQ(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push8(c.src, b != a))
GTH(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push8(c.src, b > a))
LTH(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push8(c.src, b < a))
JMP(c::CPU)::Void = (a = pop(c.src); warp(u, a))
JCN(c::CPU)::Void = (a = pop(c.src); (pop8(c.src) && warp(u, a)))
JSR(c::CPU)::Void = (a = pop(c.src); push16(c.dst, c.ram.ptr); warp(u, a))
STH(c::CPU)::Void = (a = pop(c.src); push(c.dst, a))

#= Memory =#
LDZ(c::CPU)::Void = (a = pop8(c.src); push(c.src, peek(c.ram.dat, a)))
STZ(c::CPU)::Void = (a = pop8(c.src); b = pop(c.src); poke(c.ram.dat, a, b))
LDR(c::CPU)::Void = (a = pop8(c.src); push(c.src, peek(c.ram.dat, c.ram.ptr + Int8(a))))
STR(c::CPU)::Void = (a = pop8(c.src); b = pop(c.src); poke(c.ram.dat, c.ram.ptr + Int8(a), b))
LDA(c::CPU)::Void = (a = pop16(c.src); push(c.src, peek(c.ram.dat, a)))
STA(c::CPU)::Void = (a = pop16(c.src); b = pop(c.src); poke(c.ram.dat, a, b))
DEI(c::CPU)::Void = (a = pop8(c.src); push(c.src, devr(c.devs[a >> 4], a)))
DEO(c::CPU)::Bool = (a = pop8(c.src); b = pop(c.src); (!devw(c.devs[a >> 4], a, b) && true))

#= Arithmetic =#
ADD(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, b + a))
SUB(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, b - a))
MUL(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, b * a))
DIV(c::CPU)::Void = begin
  a = pop(c.src); b = pop(c.src); (a == 0 && (c.src.error = 3; a = 1)); push(c.src, b / a)
end
AND(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, b & a))
ORA(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, b | a))
EOR(c::CPU)::Void = (a = pop(c.src); b = pop(c.src); push(c.src, b ^ a))
SFT(c::CPU)::Void = (a = pop8(c.src); b = pop(c.src); push(c.src, b >> (a & 0x0f) << ((a & 0xf0) >> 4)))

#= Core =#
function uxn_eval!(c::CPU, vec::UInt16, fault_handler::Function)::Int
  (!bool(vec) || bool(c.devs[0].dat[0xf])) && return 0
  c.ram.ptr = vec; c.wst.ptr > 0xf8 && (c.wst.ptr = 0xf8)

  while (instr = c.ram.dat[c.ram.ptr]; c.ram.ptr += 1; bool(instr))
    #= Return mode =#
    bool(instr & MODE_RETURN) ? (c.src = c.rst; c.dst = c.wst) :
                                (c.src = c.wst;   c.dst = c.rst)

    #= Keep mode =#
    pop8 = bool(instr & MODE_KEEP) ? (c.src.kptr = c.src.ptr; pop8k) : pop8d

    #= Short mode =#
    if bool(instr & MODE_SHORT)
      push = push16; pop  = pop16;  poke = poke16; peek = peek16
      devw = devw16; devr = devr16; pull = pull16

    else
      push = push8; pop  = pop8;  poke = poke8; peek = peek8
      devw = devw8; devr = devr8; pull = pull8
    end

    @match (instr & 0x1f) begin
      #= Stack =#
      0x00 => LIT(c); 0x01 => INC(c); 0x02 => POP(c); 0x03 => DUP(c)
      0x04 => NIP(c); 0x05 => SWP(c); 0x06 => OVR(c); 0x07 => ROT(c)

      #= Logic =#
      0x08 => EQU(c); 0x09 => NEQ(c); 0x0a => GTH(c); 0x0b => LTH(c)
      0x0c => JMP(c); 0x0d => JCN(c); 0x0e => JSR(c); 0x0f => STH(c)

      #= Memory =#
      0x10 => LDZ(c); 0x11 => STZ(c); 0x12 => LDR(c); 0x13 => STR(c)
      0x14 => LDA(c); 0x15 => STA(c); 0x16 => DEI(c); 0x17 => DEO(c)

      #= Arithmetic =#
      0x18 => ADD(c); 0x19 => SUB(c); 0x1a => MUL(c); 0x1b => DIV(c)
      0x1c => AND(c); 0x1d => ORA(c); 0x1e => EOR(c); 0x1f => SFT(c)
    end

    bool(c.wst.error) && return fault_handler(c, c.wst.error, "Working-stack", instr)
    bool(c.rst.error) && return fault_handler(c, c.rst.error, "Return-stack", instr)
  end

  return 1
end

reboot!(s::Stack) ::Stack  = (s.ptr = s.kptr = s.error = 0x0; fill!(s.dat, 0x0); s)
reboot!(m::Memory)::Memory = (m.ptr = 0x0; fill!(m.dat, 0x0); m)
reboot!(d::Device)::Device = (d.addr = d.mem = vector = 0x0; fill!(d.dat, 0x0); d)
reboot!(c::CPU)::CPU = (foreach([c.wst, c.rst, c.src, c.dst, c.ram, c.devs...], reboot!); u)

uxn_eval!(fault_handler::Function, c::CPU, vector::UInt16)::Int = uxn_eval!(c, vector, fault_handler)
uxn_eval!(c::CPU, vector::UInt16)::Int = begin
  uxn_eval!(c, vector) do c, error, message, instr
    @error "number: $error; message: $message; instruction: $instr"
  end
end

end  # module
