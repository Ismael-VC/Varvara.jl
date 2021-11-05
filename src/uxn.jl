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

export  CPU, AbstractCPU,  Stack,  Memory,      Device,    uxn_eval, OVector,
  Void, PAGE_PROGRAM, MODE_SHORT,  MODE_RETURN, MODE_KEEP, uxn_boot,

  push, pop, poke, peek, devw, devr, pull, warp,

  LIT, INC, POP, DUP, NIP, SWP, OVR, ROT, EQU, NEQ, GTH, LTH, JMP, JCN, JSR, STH,
  LDZ, STZ, LDR, STR, LDA, STA, DEI, DEO, ADD, SUB, MUL, DIV, AND, ORA, EOR, SFT

#! format: on

const PAGE_PROGRAM = 0x0100
const MODE_SHORT = 0x20
const MODE_RETURN = 0x40
const MODE_KEEP = 0x80
const Void = Nothing
const OVector{T} = OffsetVector{T,Vector{T}} where {T}
const BytSht = Union{UInt8,UInt16}

OVector{T}(len::BytSht) where {T} = OffsetVector(Vector{T}(undef, len), 0:len-1)
OVector(T::DataType, len::BytSht) = OffsetVector(zeros(T, len), 0:len-1)


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

abstract type AbstractCPU end

mutable struct Device{C<:AbstractCPU,F<:Function}
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


Stack()::Stack = Stack(0, 0, 0, OVector(UInt8, 0xff))
Memory()::Memory = Memory(0, OVector(UInt16, 0xffff))
CPU()::CPU = CPU(uuid4(), Stack(), Stack(), Stack(), Stack(), Memory(), OVector{Device}(0x10))

function Device(talkfn::F, c::C, id::UInt8)::Device where {C<:AbstractCPU,F<:Function}
  Device(c, id, talkfn)
end

function Device(c::C, id::UInt8, talkfn::F)::Device where {C<:AbstractCPU,F<:Function}
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
push(s::Stack, a::UInt16)::Void = (push(s, high(a)); push(s, low(a)))
push(s::Stack, a::UInt8)::Void = begin
  s.ptr == 0xff ? (s.error = 2; return) : (s.dat[s.ptr] = a; s.ptr += 1; return)
end

pop(s::Stack, keep::Bool) = pop(s, UInt8, keep)
pop(s::Stack, mode::Type{UInt8}, keep::Bool = false)::UInt8 = begin
  keep ? (s.kptr == 0x0 ? (s.error = 1; return 0x0) : (s.kptr -= 1; return s.dat[s.kptr])) :
  (s.ptr == 0x0 ? (s.error = 1; return 0x0) : (s.ptr -= 1; return s.dat[s.ptr]))
end

pop(s::Stack, mode::Type{UInt16}, keep::Bool = false)::UInt16 = begin
  a = pop(s, keep)
  b = pop(s, keep)
  concat(a, b)
end

const StkMem = Union{Stack,Memory}

poke(sm::StkMem, a::BytSht, b::UInt8)::Void = (sm.dat[a] = b; return)
poke(sm::StkMem, a::BytSht, b::UInt16)::Void = (poke(sm, a, high(b)); poke(sm, a + 0x1, low(b)))

peek(sm::StkMem, a::UInt8)::UInt8 = sm.dat[a]
peek(sm::StkMem, a::UInt16)::UInt16 = concat(peek(sm, low(a)), peek(sm, low(a) + 0x1))

devw(d::Device, a::UInt8, b::UInt8)::Int = (d.dat[a&0xf] = b; d.talk(d, a & 0x0f, 0x1))
devw(d::Device, a::UInt8, b::UInt16)::Int = devw(d, a, high(b)) && devw(d, a + 0x1, low(b))

devr(d::Device, a::UInt8)::UInt8 = devr(d, a, UInt8)
devr(d::Device, a::UInt8, mode::Type{UInt8})::UInt8 = (d.talk(d, a & 0x0f, 0x0); return d.dat[a&0xf])
devr(d::Device, a::UInt8, mode::Type{UInt16})::UInt16 = concat(devr(d, a), devr(d, a + 0x1))

pull(c::CPU) = pull(c, UInt8)
pull(c::CPU, mode::Type{UInt8})::Void = (push(c.src, peek(c.ram, c.ram.ptr)); c.ram.ptr += 1; return)
pull(c::CPU, mode::Type{UInt16})::Void = (push(c.src, peek(c.ram, c.ram.ptr)); c.ram.ptr += 2; return)

warp(c::CPU, a::BytSht)::Void = (c.ram.ptr += a; return)


#= Core =#
function uxn_eval(c::CPU, vec::UInt16, uxn_halt!::Function)::Int
  (!bool(vec) || bool(c.dev[0].dat[0xf])) && return 0
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
      0x01 => #= INC =# (a = pop(c.src, m, k); push(c.src, a + 0x1))
      0x02 => #= POP =# pop(c.src, m, k)
      0x03 => #= DUP =# (a = pop(c.src, m, k); push(c.src, a); push(c.src, a))
      0x04 => #= NIP =# (a = pop(c.src, m, k); pop(c.src, m, k); push(c.src, a))
      0x05 => #= SWP =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, a); push(c.src, b))
      0x06 => #= OVR =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b); push(c.src, a); push(c.src, b))
      0x07 => #= ROT =# begin
                          a = pop(c.src, m, k); b = pop(c.src, m, k); c = pop(c.src, m, k)
                          push(c.src, b); push(c.src, a); push(c.src, c)
                        end

      #= Logic =#
      0x08 => #= EQU =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b == a))
      0x09 => #= NEQ =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b != a))
      0x0a => #= GTH =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b > a))
      0x0b => #= LTH =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b < a))
      0x0c => #= JMP =# (a = pop(c.src, m, k); warp(c, a))
      0x0d => #= JCN =# (a = pop(c.src, m, k); (pop(c.src, m, k) && warp(c, a)))
      0x0e => #= JSR =# (a = pop(c.src, m, k); push(c.dst, c.ram.ptr); warp(c, a))
      0x0f => #= STH =# (a = pop(c.src, m, k); push(c.dst, a))

      #= Memory =#
      0x10 => #= LDZ =# (a = pop(c.src, m, k); push(c.src, peek(c.ram.dat, a)))
      0x11 => #= STZ =# (a = pop(c.src, m, k); b = pop(c.src, m, k); poke(c.ram.dat, a, b))
      0x12 => #= LDR =# (a = pop(c.src, m, k); push(c.src, peek(c.ram.dat, c.ram.ptr + Int8(a))))
      0x13 => #= STR =# (a = pop(c.src, m, k); b = pop(c.src, m, k); poke(c.ram.dat, c.ram.ptr + Int8(a), b))
      0x14 => #= LDA =# (a = pop(c.src, m, k); push(c.src, peek(c.ram.dat, a)))
      0x15 => #= STA =# (a = pop(c.src, m, k); b = pop(c.src, m, k); poke(c.ram.dat, a, b))
      0x16 => #= DEI =# (a = pop(c.src, m, k); push(c.src, devr(c.dev[a >> 4], a, m)))
      0x17 => #= DEO =# (a = pop(c.src, m, k); b = pop(c.src, m, k); !bool(devw(c.dev[a >> 4], a, b)) && return 1)

      #= Arithmetic =#
      0x18 => #= ADD =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b + a))
      0x19 => #= SUB =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b - a))
      0x1a => #= MUL =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b * a))
      0x1b => #= DIV =# begin
                          a = pop(c.src, m, k); b = pop(c.src, m, k)
                          a == 0 && (c.src.error = 3; a = 1)
                          push(c.src, b / a)
                        end
      0x1c => #= AND =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b & a))
      0x1d => #= ORA =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b | a))
      0x1e => #= EOR =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b ^ a))
      0x1f => #= SFT =# (a = pop(c.src, m, k); b = pop(c.src, m, k); push(c.src, b >> (a & 0x0f) << ((a & 0xf0) >> 4)))
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
