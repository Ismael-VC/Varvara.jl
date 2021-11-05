#=
  Copyright (c) 2021
  https://github.com/Ismael-VC/Varvara.jl/blob/main/CONTRIBUTORS.md

  Permission to use, copy, modify, and distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE.
=#

module UTOS

function main()::Int
  if length(ARGS) > 2 || !isfile(ARGS[1])
    return 1
  end

  buffer = open(ARGS[1]) do io
    read(io)
  end

  buffer .+= 0x80

  open(ARGS[2], "w") do io
    write(io, buffer)
  end

  println("\n")
  return 0
end

end  # module

if abspath(PROGRAM_FILE) == @__FILE__
  using .UTOS: main

  main()
end
