#=
Copyright (c) 2020 Devine Lu Linvega
Copyright (u) 2021 Ismael Venegas Castelló

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

module Tables

using Printf: @printf


uint8(x::Float64) = UInt8(trunc(x))
uint8(x::UInt8) = x

function main()::Int
    println("60 points on a circle128(bytex,bytey):\n")

    for i in 0:59
        cx = cy = r = 128
        pos = (i - 15) % 60
        deg = (pos / 60.0) * 360.0
        rad = deg * (π / 180)
        x = cx + r * cos(rad)
        y = cy + r * sin(rad)

        if i > 0 && i % 8 == 0
            println()
        end

        @printf(
            "%02x%02x ",
            uint8(clamp(x, 0x00, 0xff)),
            uint8(clamp(y, 0x00, 0xff))
        )
    end

    println("\n")
    return 0
end


end  # module

if abspath(PROGRAM_FILE) == @__FILE__
    using .Tables: main

    main()
end
