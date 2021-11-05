#=
  Copyright (c) 2021
  https://github.com/Ismael-VC/Varvara.jl/blob/main/CONTRIBUTORS.md

  Permission to use, copy, modify, and distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE.
=#

module VarvaraUtils

export low, high, concat, bool

low(x::UInt16)::UInt8 = x & 0xff
high(x::UInt16)::UInt8 = x >> 0x8
concat(low::UInt8, high::UInt8)::UInt16 = UInt16(high) << 8 | low
bool(x::Number)::Bool = !iszero(x)

for T ∈ (:AbstractArray, :AbstractString)
  @eval bool(x::$T) = !isempty(x)
end

end  # module
