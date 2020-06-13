# A small stack-based processor

This project serves as a proof-of-concept for a larger 32-bit stack-based processor.

Everything in this repository is MIT-licensed.

## Overview

 - Stack-based
 - No registers
 - 8-bit words, 8-bit instructions, 8-bit IP
 - Harvard architecture
 - Stack is not memory-addressable
 - 10½ instructions total

## Instruction set

```
Byte    Name        Action/Comment
----------------------------------
0x00    send        ( value -- ) [value is sent to the screen]
0x01    jmp         ( ip -- )
0x02    skip        ( cond -- ) [ if cond & 0x80 then skip the next instruction byte ]
0x03    drop        ( a -- )
0x04    add         ( a b -- a + b )    [ALU]
0x05    not         ( a -- ~a )         [ALU]
0x06    swap        ( a b -- b a )      [ALU]
0x07    dup         ( a -- a a )
0x08    ressurect   ( -- a ) [only legal soon after a drop]
0x09    imm         ( -- x ) [single-byte immediate follows the instruction]
others  nop         ( -- )
```

I don't like `ressurect`, since it's really hacky, but it's the simplest way to make it
possible to do stack rotations.

## Goals

The goal for this is a MVP stack-based processor, with:
 - A high-level concatenative assembler
 - A virtual machine
 - A hardware FPGA implementation

## Can it do the things?

Yes! Despite the small number of instructions, it can do a surprising amount.

### Function calls

Call `f`:
```
    imm next
    imm f
    jmp
next:
```
Since the address is on the stack, `ret` is just `jmp`.

### Conditional jumps

```
    ; Stack: condition
    imm dest
    swap
    skip jmp
    drop ; If jmp wasn't taken, get rid of the destination
```

### Conditionally choose a value

```
    ; Stack: A B condition
    skip swap
    drop
    ; Stack: condition ? B : A
```

### Rot3

```
    ; Stack: A B C
    swap    ; Stack: A C B
    drop    ; Stack: A C [B]
    swap    ; Stack: C A [B]
    ressurect
    ; Stack: C A B
```
The other direction is done similarly.

### Subtraction

Subtraction is just adding the inverse

```
    ; Stack: A B
    not
    ; Stack: A ~B
    imm 1 add
    ; Stack: A -B
    add
```
