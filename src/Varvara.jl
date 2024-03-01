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

#! format: off

#= Utils =#
include("vvutils.jl");     @reexport using .VarvaraUtils
  
#= Uxn =#
include("uxn.jl");         @reexport using .Uxn
include("uxntal.jl");      @reexport using .UxntalLang
# include("uxnasm.jl");      @reexport using .UxnASM

#= Varvara =#
include("vvos.jl");        @reexport using .VarvaraOS
include("vvbios.jl");      @reexport using .VarvaraBIOS

#= CLI =#
include("uxncli.jl");      @reexport using .UxnCLI
# include("uxnrepl.jl");     @reexport using .UxnREPL

#= GUI =#
# include("devices/apu.jl"); @reexport using .APU
# include("devices/ppu.jl"); @reexport using .PPU
# include("uxnemu.jl");      @reexport using .UxnEMU

#= Debugger =#
# include("uxndbg.jl");      @reexport using .UxnDBG
# include("uxngdbg.jl");     @reexport using .UxnDBGG

#= Documentation =#
# include("vvdocs.jl");      @reexport using .VarvaraDocs

#! format: on


end  # module



# using Varvara

#=
function main()
  c = CPU()
  uxn_boot(c)
end

main()
=#
