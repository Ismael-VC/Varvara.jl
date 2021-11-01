module UxnUtils

export low_byte, high_byte, concat_bytes, bool

low_byte(x::UInt16)::UInt8 = x & 0xff
high_byte(x::UInt16)::UInt8 = x >> 0x8
concat_bytes(a::UInt8, b::UInt8)::UInt16 = UInt16(b) << 8
bool(x::Number)::Bool = !iszero(x)

end  # module
