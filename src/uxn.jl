#=
Copyright (c) 2021 Devine Lu Linvega, Andrew Alderwick, Ismael Venegas Castelló

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

module Uxn; using Match, OffsetArrays, .Utils

export UxnCPU, AbstractCPU, Stack, Memory, Device, compute!, OVector, Void, PAGE_PROGRAM, MODE_SHORT, MODE_RETURN,
  MODE_KEEP, reset!, bool, push8, pop8k, pop8d, poke8, peek8, devw8, devr8, pull8, warp, push16, pop8d, pop16d, poke16,
  peek16, devw16, devr16, pull16, pop16, LIT, INC, POP, DUP, NIP, SWP, OVR, ROT, EQU, NEQ, GTH, LTH, JMP, JCN, JSR, STH,
  LDZ, STZ, LDR, STR, LDA, STA, DEI, DEO, ADD, SUB, MUL, DIV, AND, ORA, EOR, SFT

const PAGE_PROGRAM = 0x0100; const MODE_SHORT = 0x20; const MODE_RETURN = 0x40; const MODE_KEEP = 0x80
const Void = Nothing; const OVector{T} = OffsetVector{T, Vector{T}} where {T}
OVector{T}(len::Integer) where {T} = OffsetVector(Vector{T}(undef, len), 0:len-1)

mutable struct Stack
  ptr::UInt8; kptr::UInt8; error::UInt8; dat::OVector{UInt8}
end
mutable struct Memory
  ptr::UInt16; dat::OVector{UInt8}
end
abstract type AbstractCPU end; mutable struct Device{C<:AbstractCPU, F<:Function}
  cpu::C; id_addr::Int; vector::UInt16; talk::F; mem::OVector{UInt8}; dat::OVector{UInt8}
end
mutable struct UxnCPU <: AbstractCPU
  wst::Stack; rst::Stack; src::Stack; dst::Stack; ram::Memory; dev::OVector{Device}
end

Stack()::Stack = Stack(0, 0, 0, OVector{UInt8}(0xff))
Memory()::Memory = Memory(0, OVector{UInt8}(0xffff))
UxnCPU()::UxnCPU = UxnCPU(Stack(), Stack(), Stack(), Stack(), Memory(), OVector{Device}(0x10))
(Device(talkfn::F, cpu::C, id_addr::Int)::Device) where {C<:AbstractCPU, F<:Function} = Device(cpu, id_addr, talkfn)
(Device(cpu::C, id_addr::Int, talkfn::F)::Device) where {C<:AbstractCPU, F<:Function} = begin
  d = Device(cpu, id_addr, 0x000, talkfn, cpu.ram.dat, OVector{UInt8}(0x10))
  cpu.dev[id_addr] = d
  return d
end
(Device(cpu::C, id_addr::Int)::Device) where {C<:AbstractCPU} = begin
  Device(cpu, id_addr) do d::Device, b0::UInt8, w::UInt8
    return true
  end
end

reset!(s::Stack)::Stack = (s.ptr = s.kptr = s.error = 0x0; fill!(s.dat, 0x0); s)
reset!(m::Memory)::Memory = (m.ptr = 0x0; fill!(m.dat, 0x0); m)
reset!(d::Device) = (d.addr = d.mem = vector = 0x0; fill!(d.dat, 0x0); d)
reset!(cpu::UxnCPU) = (foreach([cpu.wst, cpu.rst, cpu.src, cpu.dst, cpu.ram, cpu.dev...], reset!); u)

#= Byte mode =#
push8(s::Stack, a::UInt8)::Void = s.ptr == 0xff ? (s.error = 2; return) : (s.dat[s.ptr] = a; s.ptr += 1; return)
pop8k(s::Stack)::UInt8 = s.kptr == 0x0 ? (s.error = 0x1; return 0x0) : (s.kptr -= 1; return s.dat[s.kptr])
pop8d(s::Stack)::UInt8 = s.ptr == 0x0 ? (s.error = 0x1; return 0x0) : (s.ptr -= 1; return s.dat[s.ptr])
poke8(m::AbstractVector{UInt8}, a::Integer, b::UInt8)::Void = (m[a] = b; return)
peek8(m::AbstractVector{UInt8}, a::Integer)::UInt8 = m[a]
devw8(d::Device, a::UInt8, b::UInt8)::Bool = (d.dat[a & 0xf] = b; d.talk(d, a & 0x0f, true))
devr8(d::Device, a::UInt8)::UInt8 = (d.talk(d, a & 0x0f, false); return d.dat[a & 0xf])
warp(cpu::UxnCPU, a::Integer)::Void = (cpu.ram.ptr += a; return)
pull8(cpu::UxnCPU)::Void = (push8(cpu.src, peek8(cpu.ram.dat, cpu.ram.ptr)); cpu.ram.ptr += 1; return)
#= Short mode =#
push16(s::Stack, a::UInt16)::Void = (push8(s, high_byte(a)); push8(s, low_byte(a)))
pop16(s::Stack)::UInt16 = ((a = pop8(s), b = pop8(s)); a + b << 8)
poke16(m::AbstractVector{UInt8}, a::Integer, b::UInt16)::Void = (poke8(m, a, b >> 8); poke8(m, a + 0x1, b))
peek16(m::AbstractVector{UInt8}, a::Integer)::UInt16 = (peek8(m, a) << 8) + peek8(m, a + 0x1)
devw16(d::Device, a::UInt8, b::UInt16)::Bool = devw8(d, a, b >> 8) && devw8(d, a + 0x1, b)
devr16(d::Device, a::UInt8)::UInt16 = (devr8(d, a) << 8) + devr8(d, a + 0x1)
pull16(cpu::UxnCPU)::Void = (push16(cpu.src, peek16(cpu.ram.dat, cpu.ram.ptr)); cpu.ram.ptr += 2; return)
#= Stack =#
LIT(cpu::UxnCPU)::Void = pull(u)
INC(cpu::UxnCPU)::Void = (a = pop(cpu.src); push(cpu.src, a + 0x1))
POP(cpu::UxnCPU) = pop(cpu.src)
DUP(cpu::UxnCPU)::Void = (a = pop(cpu.src); push(cpu.src, a); push(cpu.src, a))
NIP(cpu::UxnCPU)::Void = (a = pop(cpu.src); pop(cpu.src); push(cpu.src, a))
SWP(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, a); push(cpu.src, b))
OVR(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, b); push(cpu.src, a); push(cpu.src, b))
ROT(cpu::UxnCPU)::Void = begin
  a = pop(cpu.src); b = pop(cpu.src); c = pop(cpu.src); push(cpu.src, b); push(cpu.src, a); push(cpu.src, c)
end
#= Logic =#
EQU(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push8(cpu.src, b == a))
NEQ(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push8(cpu.src, b != a))
GTH(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push8(cpu.src, b > a))
LTH(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push8(cpu.src, b < a))
JMP(cpu::UxnCPU)::Void = (a = pop(cpu.src); warp(u, a))
JCN(cpu::UxnCPU)::Void = (a = pop(cpu.src); (pop8(cpu.src) && warp(u, a)))
JSR(cpu::UxnCPU)::Void = (a = pop(cpu.src); push16(cpu.dst, cpu.ram.ptr); warp(u, a))
STH(cpu::UxnCPU)::Void = (a = pop(cpu.src); push(cpu.dst, a))
#= Memory =#
LDZ(cpu::UxnCPU)::Void = (a = pop8(cpu.src); push(cpu.src, peek(cpu.ram.dat, a)))
STZ(cpu::UxnCPU)::Void = (a = pop8(cpu.src); b = pop(cpu.src); poke(cpu.ram.dat, a, b))
LDR(cpu::UxnCPU)::Void = (a = pop8(cpu.src); push(cpu.src, peek(cpu.ram.dat, cpu.ram.ptr + Int8(a))))
STR(cpu::UxnCPU)::Void = (a = pop8(cpu.src); b = pop(cpu.src); poke(cpu.ram.dat, cpu.ram.ptr + Int8(a), b))
LDA(cpu::UxnCPU)::Void = (a = pop16(cpu.src); push(cpu.src, peek(cpu.ram.dat, a)))
STA(cpu::UxnCPU)::Void = (a = pop16(cpu.src); b = pop(cpu.src); poke(cpu.ram.dat, a, b))
DEI(cpu::UxnCPU)::Void = (a = pop8(cpu.src); push(cpu.src, devr(cpu.dev[a >> 4], a)))
DEO(cpu::UxnCPU)::Bool = (a = pop8(cpu.src); b = pop(cpu.src); (!devw(cpu.dev[a >> 4], a, b) && true))
#= Arithmetic =#
ADD(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, b + a))
SUB(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, b - a))
MUL(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, b * a))
DIV(cpu::UxnCPU)::Void = begin
  a = pop(cpu.src); b = pop(cpu.src); (a == 0 && (cpu.src.error = 3; a = 1)); push(cpu.src, b / a)
end
AND(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, b & a))
ORA(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, b | a))
EOR(cpu::UxnCPU)::Void = (a = pop(cpu.src); b = pop(cpu.src); push(cpu.src, b ^ a))
SFT(cpu::UxnCPU)::Void = (a = pop8(cpu.src); b = pop(cpu.src); push(cpu.src, b >> (a & 0x0f) << ((a & 0xf0) >> 4)))

#= Core =#
function compute!(cpu::UxnCPU, vec::UInt16, fault_handler::Function)::Int
  (!bool(vec) || bool(cpu.dev[0].dat[0xf])) && return 0
  cpu.ram.ptr = vec; cpu.wst.ptr > 0xf8 && (cpu.wst.ptr = 0xf8)
  while (instr = cpu.ram.dat[cpu.ram.ptr]; cpu.ram.ptr += 1; bool(instr))
    #= Return mode =#
    bool(instr & MODE_RETURN) ? (cpu.src = cpu.rst; cpu.dst = cpu.wst) : (cpu.src = cpu.wst; cpu.dst = cpu.rst)
    #= Keep mode =#
    pop8 = bool(instr & MODE_KEEP) ? (cpu.src.kptr = cpu.src.ptr; pop8k) : pop8d
    #= Short mode =#
    if bool(instr & MODE_SHORT)
      push = push16; pop = pop16; poke = poke16; peek = peek16; devw = devw16; devr = devr16; pull = pull16
    else
      push = push8; pop = pop8; poke = poke8; peek = peek8; devw = devw8; devr = devr8; pull = pull8
    end
    @match (instr & 0x1f) begin
      #= Stack =#
      0x00 => LIT(u); 0x01 => INC(u); 0x02 => POP(u); 0x03 => DUP(u)
      0x04 => NIP(u); 0x05 => SWP(u); 0x06 => OVR(u); 0x07 => ROT(u)
      #= Logic =#
      0x08 => EQU(u); 0x09 => NEQ(u); 0x0a => GTH(u); 0x0b => LTH(u)
      0x0c => JMP(u); 0x0d => JCN(u); 0x0e => JSR(u); 0x0f => STH(u)
      #= Memory =#
      0x10 => LDZ(u); 0x11 => STZ(u); 0x12 => LDR(u); 0x13 => STR(u)
      0x14 => LDA(u); 0x15 => STA(u); 0x16 => DEI(u); 0x17 => DEO(u)
      #= Arithmetic =#
      0x18 => ADD(u); 0x19 => SUB(u); 0x1a => MUL(u); 0x1b => DIV(u)
      0x1c => AND(u); 0x1d => ORA(u); 0x1e => EOR(u); 0x1f => SFT(u)
    end
    bool(cpu.wst.error) && return fault_handler(u, cpu.wst.error, "Working-stack", instr)
    bool(cpu.rst.error) && return fault_handler(u, cpu.rst.error, "Return-stack", instr)
  end
  return 1
end

compute!(fault_handler::Function, cpu::UxnCPU, vector::UInt16)::Int = compute!(cpu, vector, fault_handler)
compute!(cpu::UxnCPU, vector::UInt16)::Int = begin
  compute!(cpu, vector) do cpu, error, message, instr
    @error "number: $error; message: $message; instruction: $instr"
  end
end

end  # module
