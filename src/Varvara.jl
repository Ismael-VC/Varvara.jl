module Varvara

using Reexport: @reexport

include("varvara-utils.jl")
include("uxn.jl")
include("varvara-os.jl")
include("uxn-cli.jl")
include("uxn-dbg.jl")
include("uxn-dbgg.jl")
include("uxn-repl.jl")
include("devices/apu.jl")
include("devices/ppu.jl")
include("uxn-emu.jl")
include("uxn-asm.jl")
include("varvara-docs.jl")

@reexport using .UxnUtils
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
