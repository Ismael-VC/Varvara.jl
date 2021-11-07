#=
  Copyright (c) 2021
  https://github.com/Ismael-VC/Varvara.jl/blob/main/CONTRIBUTORS.md

  Permission to use, copy, modify, and distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE.
=#

module UxnREPL

using ..Uxn: CPU
using ..VarvaraOS: uxn_boot

using ReplMaker: initrepl

#! format: off

function uxntal_repl_v1(s)
  c = CPU()
  address = c.ram.
  open("snarf.tmp", "w") do f
    ss = "|10 @Console [ &vector \$2 &read \$1 &pad \$5 &write \$1 &error \$1 ]" *
         " %EMIT { .Console/write DEO } %NL { #0a EMIT } |0100 " * s
    write(f, ss)
  end
  @info "file saved"
  read(`uxnasm snarf.tmp snarf.rom`)
  push!(ARGS, "snarf.rom")
  uxn_boot(c)
  c
end

if isdefined(Base, :active_repl)
  try
    initrepl(
      uxntal_repl_v1, 
      prompt_text = "uxntal> ",
      prompt_color = :blue, 
      start_key = ')',
      mode_name = "Uxntal"
    )
  catch e
    @show e
  end
end

#! format: on

end  # module

