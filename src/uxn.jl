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

using ..VarvaraUtils: high, low, concat, bool

#! format: off

export  CPU,  AbstractCPU,  Stack,  Memory,   Device,  uxn_eval, ByteVector,
        Void, PAGE_PROGRAM, MODE_SHORT,  MODE_RETURN, MODE_KEEP, uxn_boot,

        push, pop, poke, peek, devw, devr, pull, warp

#! format: on

const PAGE_PROGRAM = 0x0100
const MODE_SHORT = 0x20
const MODE_RETURN = 0x40
const MODE_KEEP = 0x80
const Void = Nothing
const ByteVector{UInt8} = OffsetVector{UInt8,Vector{UInt8}}
const UInt8_16 = Union{UInt8,UInt16}

ByteVector{T}(len::UInt8_16) where {T} = OffsetVector(Vector{T}(undef, len), 0:len-1)
ByteVector(len::UInt8_16) = OffsetVector(zeros(UInt8, len), 0:len-1)


mutable struct Stack
  ptr::UInt8
  kptr::UInt8
  error::UInt8
  dat::ByteVector{UInt8}
end

mutable struct Memory
  ptr::UInt16
  dat::ByteVector{UInt8}
end

abstract type AbstractCPU end

mutable struct Device{C<:AbstractCPU,F<:Function}
  c::C
  id::UInt8
  vec::UInt16
  talk::F
  mem::ByteVector{UInt8}
  dat::ByteVector{UInt8}
end

mutable struct CPU <: AbstractCPU
  uuid::UUID
  wst::Stack
  rst::Stack
  src::Stack
  dst::Stack
  ram::Memory
  dev::ByteVector{Device}
end


Stack()::Stack = Stack(0, 0, 0, ByteVector(0xff))
Memory()::Memory = Memory(0, ByteVector(0xffff))
CPU()::CPU = CPU(uuid4(), Stack(), Stack(), Stack(), Stack(), Memory(), ByteVector{Device}(0x10))

function Device(talkfn::F, c::C, id::UInt8)::Device where {C<:AbstractCPU,F<:Function}
  Device(c, id, talkfn)
end

function Device(c::C, id::UInt8, talkfn::F)::Device where {C<:AbstractCPU,F<:Function}
  d = Device(c, id, 0x000, talkfn, c.ram.dat, ByteVector(0x10))
  c.dev[id] = d
  return d
end

function Device(c::C, id::UInt8)::Device where {C<:AbstractCPU}
  Device(c, id) do d::Device, b0::UInt8, w::UInt8
    return true
  end
end

#= Operations =#
push(s::Stack, a::UInt16)::Void = (push(s, high(a)); push(s, low(a)))
push(s::Stack, a::UInt8)::Void = begin
  s.ptr == 0xff ? (s.error = 2; return) : (s.dat[s.ptr] = a; s.ptr += 1; return)
end

#! format: off

pop(s::Stack,  keep::Bool, mode::Type{UInt8})::UInt8 = begin
  keep ? 
    (s.kptr == 0x0 ? (s.error = 1; return 0x0) : (s.kptr -= 1; return s.dat[s.kptr])) :
    (s.ptr == 0x0 ? (s.error = 1; return 0x0) : (s.ptr -= 1; return s.dat[s.ptr]))
end

pop(s::Stack,  keep::Bool, mode::Type{UInt16})::UInt16 = begin
  a = pop(s, keep, UInt8)
  b = pop(s, keep, UInt8)
  concat(a, b)
end

#! format: on

poke(m::Memory, a::UInt16, b::UInt8)::Void = (m.dat[a] = b; return)
poke(m::Memory, a::UInt16, b::UInt16)::Void = (poke(m, a, low(b)); poke(m, a + 0x1, high(b)))

peek(m::Memory, a::UInt16, mode::Type{UInt8})::UInt8 = m.dat[a]
peek(m::Memory, a::UInt16, mode::Type{UInt16})::UInt16 = concat(peek(m, a, UInt8), peek(m, a + 0x1, UInt8))

devw(d::Device, a::UInt8, b::UInt8)::Int = (d.dat[a&0xf] = b; d.talk(d, a & 0x0f, 0x1))
devw(d::Device, a::UInt8, b::UInt16)::Int = devw(d, a, low(b)) && devw(d, a + 0x1, high(b))

devr(d::Device, a::UInt8, mode::Type{UInt8})::UInt8 = (d.talk(d, a & 0x0f, 0x0); return d.dat[a&0xf])
devr(d::Device, a::UInt8, mode::Type{UInt16})::UInt16 = concat(devr(d, a, UInt8), devr(d, a + 0x1, UInt8))

pull(c::CPU, mode::Type{UInt8})::Void = (push(c.src, peek(c.ram, c.ram.ptr, mode)); c.ram.ptr += 1; return)
pull(c::CPU, mode::Type{UInt16})::Void = (push(c.src, peek(c.ram, c.ram.ptr, mode)); c.ram.ptr += 2; return)

warp(c::CPU, a::UInt8)::Void = (c.ram.ptr += a; return)


#= Core =#
function uxn_eval(c::CPU, vec::UInt16, uxn_halt!::Function)::Int
  halt = c.dev[0].dat[0xf]
  (!bool(vec) || bool(halt)) && return 0
  c.ram.ptr = vec
  c.wst.ptr > 0xf8 && (c.wst.ptr = 0xf8)

  while (instr = c.ram.dat[c.ram.ptr]; c.ram.ptr += 1; bool(instr))
    # Return mode #
    bool(instr & MODE_RETURN) ? (c.src = c.rst; c.dst = c.wst) : (c.src = c.wst; c.dst = c.rst)

    # Keep mode #
    k = bool(instr & MODE_KEEP) ? (c.src.kptr = c.src.ptr; true) : false

    # Short mode #
    m = bool(instr & MODE_SHORT) ? UInt16 : UInt8

    #! format: off
    @match (instr & 0x1f) begin
      #= Stack =#
      0x00 => #= LIT =# pull(c, m)
      0x01 => #= INC =# (a = pop(c.src, k, m); push(c.src, a + 0x1))
      0x02 => #= POP =# pop(c.src, k, m)
      0x03 => #= DUP =# (a = pop(c.src, k, m); push(c.src, a); push(c.src, a))
      0x04 => #= NIP =# (a = pop(c.src, k, m); pop(c.src, k, m); push(c.src, a))
      0x05 => #= SWP =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, a); push(c.src, b))
      0x06 => #= OVR =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, b); push(c.src, a); push(c.src, b))
      0x07 => #= ROT =# begin
                          a = pop(c.src, k, m); b = pop(c.src, k, m); c = pop(c.src, k, m)
                          push(c.src, b); push(c.src, a); push(c.src, c)
                        end

      #= Logic =#
      0x08 => #= EQU =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, UInt8(b == a)))
      0x09 => #= NEQ =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, UInt8(b != a)))
      0x0a => #= GTH =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, UInt8(b > a)))
      0x0b => #= LTH =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, UInt8(b < a)))
      0x0c => #= JMP =# (a = pop(c.src, k, m); warp(c, a))
      0x0d => #= JCN =# (a = pop(c.src, k, m); (pop(c.src, k, UInt8) && warp(c, a)))
      0x0e => #= JSR =# (a = pop(c.src, k, m); push(c.dst, UInt16(c.ram.ptr)); warp(c, a))
      0x0f => #= STH =# (a = pop(c.src, k, m); push(c.dst, a))

      #= Memory =#
      0x10 => #= LDZ =# (a = pop(c.src, k, UInt8); push(c.src, peek(c.ram, a, m)))
      0x11 => #= STZ =# (a = pop(c.src, k, UInt8); b = pop(c.src, k, m); poke(c.ram, a, b))
      0x12 => #= LDR =# (a = pop(c.src, k, UInt8); push(c.src, peek(c.ram, c.ram.ptr + Int8(a), m)))
      0x13 => #= STR =# (a = pop(c.src, k, UInt8); b = pop(c.src, k, m); poke(c.ram, c.ram.ptr + Int8(a), b))
      0x14 => #= LDA =# (a = pop(c.src, k, UInt16); push(c.src, peek(c.ram, a, m)))
      0x15 => #= STA =# (a = pop(c.src, k, UInt16); b = pop(c.src, k, m); poke(c.ram, a, b))
      0x16 => #= DEI =# (a = pop(c.src, k, UInt8); push(c.src, devr(c.dev[a >> 4], a, m)))
      0x17 => #= DEO =# begin 
        a = pop(c.src, k, UInt8); 
        b = pop(c.src, k, m); 
        !bool(devw(c.dev[a >> 4], a, b)) && return 1
      end

      #= Arithmetic =#
      0x18 => #= ADD =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, b + a))
      0x19 => #= SUB =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, b - a))
      0x1a => #= MUL =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, b * a))
      0x1b => #= DIV =# begin
                          a = pop(c.src, k, m); b = pop(c.src, k, m)
                          a == 0 && (c.src.error = 3; a = 1)
                          push(c.src, b / a)
                        end
      0x1c => #= AND =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, b & a))
      0x1d => #= ORA =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, b | a))
      0x1e => #= EOR =# (a = pop(c.src, k, m); b = pop(c.src, k, m); push(c.src, b ^ a))
      0x1f => #= SFT =# begin 
                          a = pop(c.src, k, UInt8); b = pop(c.src, k, m) 
                          push(c.src, b >> (a & 0x0f) << ((a & 0xf0) >> 4))
                        end
    end 
    #! format: on

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

reset!(s::Stack)::Stack = (s.ptr = s.kptr = s.error = 0x0; fill!(s.dat, 0x0); s)
reset!(m::Memory)::Memory = (m.ptr = 0x0; fill!(m.dat, 0x0); m)
reset!(d::Device)::Device = (vec = 0x0; fill!(d.dat, 0x0); d)
reset!(c::CPU)::CPU = (reset!.([c.wst, c.rst, c.src, c.dst, c.ram, c.dev...]); c)


end  # module
