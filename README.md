# Uxn

A [stack-based VM](https://wiki.xxiivv.com/site/uxn.html), written in ANSI C.

## Assembly Syntax

### Write

- `;variable`, automatically assign an address to a label.
- `:const FFCF`, manually assign an address to a label.
- `@label`, assign the current address to a label.

### Read

- `,literal`, push label value to stack, prefixed with `LIT LEN`.
- `.pointer`, push label value to stack.

### Special

- `( comment )`, toggle parsing on/off.
- `|0010`, move to position in the program.
- `"hello`, push literal bytes for word "hello".
- `#04`, a zero-page address, equivalent to `,0004`.

### Operator modes

- `,1234 ,0001 ADD^`, 16-bits operators have the short flag `^`.
- `,12 ,11 GTH JMP?`, conditional operators have the cond flag `?`.

```
( hello world )

;iterator
:dev1r FFF0
:dev1w FFF1

|0100 @RESET

@word1 "hello_word ( len: 0x0b )

@loop
	,dev1w STR ( write to stdout )
	,incr JSR ( increment itr )
	,word1 ,strlen JSR ( get strlen )
	NEQ ,loop ROT JSR? ( loop != strlen )

BRK

@strlen
	,0001 ADD^ LDR
	RTS

@incr
	,iterator LDR
	,01 ADD
	,iterator STR 
	,iterator LDR
	RTS

|c000 @FRAME BRK 
|d000 @ERROR BRK 
|FFFA .RESET .FRAME .ERROR
```

## TODOs

### Assembler

- Implement shorthand operators
- Signed operations

### CPU

- Signed operations
- Catch overflow/underflow
- A Three-Way Decision Routine(http://www.6502.org/tutorials/compare_instructions.html)
- Draw pixel to screen
- Redo overflow/underflow mappping
- Detect mouse click
- SDL Layer Emulator
- Build PPU

### Devices

- Devices each have an input byte, an output byte and two request bytes.

## Refs

https://code.9front.org/hg/plan9front/file/a7f9946e238f/sys/src/games/nes/cpu.c
http://www.w3group.de/stable_glossar.html
http://www.emulator101.com/6502-addressing-modes.html
http://forth.works/8f0c04f616b6c34496eb2141785b4454
https://justinmeiners.github.io/lc3-vm/