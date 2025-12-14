#!/bin/bash

set -e

FILE_NAME="$1"
FILE_ASM="${FILE_NAME%.ru}.asm"
FILE_OBJ="${FILE_NAME%.ru}.o"
FILE_BIN="out.bin"

make -j
./rune build "$FILE_NAME" -o "$FILE_ASM"
nasm -f elf64 "$FILE_ASM" -o "$FILE_OBJ"
gcc -no-pie "$FILE_OBJ" -o "$FILE_BIN"
