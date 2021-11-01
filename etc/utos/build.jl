#!/usr/bin/env julia

#=
Copyright (c) 2021
https://github.com/Ismael-VC/Varvara.jl/blob/main/CONTRIBUTORS.md

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

function main()
    println("Running..")
    run(`julia utos.jl ../../projects/sounds/pad1.ss8 ../../projects/sounds/pad1.pcm`)
    println("Done.")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
