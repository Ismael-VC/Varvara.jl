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

module Varvara

using Reexport: @reexport

include("vvutils.jl")
include("uxn.jl")
include("vvos.jl")
include("uxncli.jl")
include("uxndbg.jl")
include("uxngdbg.jl")
include("uxnrepl.jl")
include("devices/apu.jl")
include("devices/ppu.jl")
include("uxnemu.jl")
include("uxnasm.jl")
include("vvdocs.jl")

@reexport using .VarvaraUtils
@reexport using .Uxn
@reexport using .VarvaraOS
@reexport using .UxnCLI
@reexport using .UxnDBG
@reexport using .UxnDBGG
@reexport using .UxnREPL
@reexport using .APU
@reexport using .PPU
@reexport using .UxnEMU
@reexport using .UxnASM
@reexport using .VarvaraDocs

end  # module



using .Varvara


function main()
  c = CPU()
  uxn_boot(c)
end

#main()