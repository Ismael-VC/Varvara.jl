#=
Copyright (c) 2021
https://github.com/Ismael-VC/Varvara.jl/blob/main/CONTRIBUTORS.md

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
=#

module UxnCLI

import  Base: show

using  Dates: year, dayofyear, month, dayofmonth, hour, minute, second, dayofweek, now
using Printf: @sprintf

import Match

using  Match: @match

using ..Uxn: CPU, Stack, Memory, Device
using ..VarvaraOS: boot!, compute!

export dev_console, dev_system, dev_datetime, dev_file,
       inspect, system_talk, nil_talk, datetime_talk,
       console_talk

show(io::IO, s::Stack)::Nothing = print(io, """
Stack:\tptr: $(@sprintf("%04x", s.ptr))\tkptr: $(@sprintf("%04x", s.kptr))\terror: $(@sprintf("%04x", s.error))
$(join([@sprintf("%02x", i) for i in s.dat[0:15]], ' '))
$(join([@sprintf("%02x", i) for i in s.dat[16:31]], ' '))
$(join([@sprintf("%02x", i) for i in s.dat[32:47]], ' '))
$(join([@sprintf("%02x", i) for i in s.dat[48:63]], ' '))
""")

show(io::IO, m::Memory)::Nothing = print(io, """
Memory:\tptr: $(@sprintf("%04x", m.ptr))
$(join([@sprintf("%02x", i) for i in m.dat[0:15]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[16:31]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[32:47]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[48:63]], ' '))

$(join([@sprintf("%02x", i) for i in m.dat[64:79]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[78:95]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[94:111]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[112:127]], ' '))

$(join([@sprintf("%02x", i) for i in m.dat[128:143]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[142:159]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[158:175]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[174:191]], ' '))

$(join([@sprintf("%02x", i) for i in m.dat[190:207]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[206:223]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[224:239]], ' '))
$(join([@sprintf("%02x", i) for i in m.dat[240:255]], ' '))
""")

show(io::IO, d::Device)::Nothing = print(io, """
$(startswith(string(d.talk), '#') ? "Empty" : titlecase(split(string(d.talk), '_')[1]))\tDevice:\t$(@sprintf("%02x", d.id))
""")

show(io::IO, c::CPU)::Nothing = print(io, """
CPU: $(c.uuid)
ram.ptr: $(@sprintf("%04x", c.ram.ptr))\tdevs: $(sum(isassigned(c.devs, i) for i in 0:15))

$(join(Device[c.devs[i] for i in 0:15 if isassigned(c.devs, i)], ""))
wst:
ptr: $(@sprintf("%04x", c.wst.ptr))\tkptr: $(@sprintf("%04x", c.wst.kptr))\terror: $(@sprintf("%04x", c.wst.error))
$(join([@sprintf("%02x", i) for i in c.wst.dat[0:15]], ' '))
$(join([@sprintf("%02x", i) for i in c.wst.dat[16:31]], ' '))
$(join([@sprintf("%02x", i) for i in c.wst.dat[32:47]], ' '))
$(join([@sprintf("%02x", i) for i in c.wst.dat[48:63]], ' '))

rst:
ptr: $(@sprintf("%04x", c.rst.ptr))\tkptr: $(@sprintf("%04x", c.rst.kptr))\terror: $(@sprintf("%04x", c.rst.error))
$(join([@sprintf("%02x", i) for i in c.rst.dat[0:15]], ' '))
$(join([@sprintf("%02x", i) for i in c.rst.dat[16:31]], ' '))
$(join([@sprintf("%02x", i) for i in c.rst.dat[32:47]], ' '))
$(join([@sprintf("%02x", i) for i in c.rst.dat[48:63]], ' '))

src:
ptr: $(@sprintf("%04x", c.src.ptr))\tkptr: $(@sprintf("%04x", c.src.kptr))\terror: $(@sprintf("%04x", c.src.error))
$(join([@sprintf("%02x", i) for i in c.src.dat[0:15]], ' '))
$(join([@sprintf("%02x", i) for i in c.src.dat[16:31]], ' '))
$(join([@sprintf("%02x", i) for i in c.src.dat[32:47]], ' '))
$(join([@sprintf("%02x", i) for i in c.src.dat[48:63]], ' '))

dst:
ptr: $(@sprintf("%04x", c.dst.ptr))\tkptr: $(@sprintf("%04x", c.dst.kptr))\terror: $(@sprintf("%04x", c.dst.error))
$(join([@sprintf("%02x", i) for i in c.dst.dat[0:15]], ' '))
$(join([@sprintf("%02x", i) for i in c.dst.dat[16:31]], ' '))
$(join([@sprintf("%02x", i) for i in c.dst.dat[32:47]], ' '))
$(join([@sprintf("%02x", i) for i in c.dst.dat[48:63]], ' '))
""")

function uxn_halt(c::CPU, err::UInt8, name::AbstractString, id::Int)::Exception
  @error "Halted"
  throw(UXN_ERRORS[error](@sprintf("%s#%04x, at 0x%04x", id, c.ram.ptr)))
end

end  # module

if abspath(PROGRAM_FILE) == @__FILE__
    using .UxnCLI: main

    main()
end
