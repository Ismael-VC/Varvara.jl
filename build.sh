#!/bin/bash

# Create bin folder
mkdir -p bin

# Assembler
clang-format -i assembler.c
rm -f ./bin/assembler
rm -f ./bin/boot.rom
cc -std=c89 -DDEBUG -Wall -Wno-unknown-pragmas -Wpedantic -Wshadow -Wextra -Werror=implicit-int -Werror=incompatible-pointer-types -Werror=int-conversion -Wvla -g -Og -fsanitize=address -fsanitize=undefined assembler.c -o bin/assembler

# Core
clang-format -i uxn.h
clang-format -i uxn.c

# Cli
clang-format -i cli.c
rm -f ./bin/cli
cc -std=c89 -DDEBUG -Wall -Wno-unknown-pragmas -Wpedantic -Wshadow -Wextra -Werror=implicit-int -Werror=incompatible-pointer-types -Werror=int-conversion -Wvla -g -Og -fsanitize=address -fsanitize=undefined uxn.c cli.c -o bin/cli

# Emulator
clang-format -i emulator.c
# rm -f ./bin/emulator
# cc -std=c89 -DDEBUG -Wall -Wno-unknown-pragmas -Wpedantic -Wshadow -Wextra -Werror=implicit-int -Werror=incompatible-pointer-types -Werror=int-conversion -Wvla -g -Og -fsanitize=address -fsanitize=undefined uxn.c emulator.c -L/usr/local/lib -lSDL2 -o bin/emulator

# run
./bin/assembler examples/hello.usm bin/boot.rom
./bin/cli bin/boot.rom