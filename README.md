# A small stack-based processor

This project serves as a proof-of-concept for an eventual larger 32-bit
stack-based processor.

Everything in this repository is MIT-licensed.

## Overview

 - Stack-based
 - No registers, no RAM
 - Combined data and call stack
 - 8-bit words, 8-bit instructions, 8-bit IP
 - Harvard architecture
 - Stack is not memory-addressable
 - 10½ instructions total

## Instruction set

*See `isa.md` for specifics.*

The instructions are `send`, `jmp`, `skip`, `swap`, `dup`, `drop`, `add`,
`not`, `imm`, and `resurrect`.

They're mostly intuitive: `send` outputs a byte, `jmp` is an unconditional
branch, and `skip` conditionally skips the next instruction byte.

`resurrect` increments the stack pointer without writing new data, which is
sometimes not UB. This makes it possible to implement stack permutations.
See `graveyard.md` for specific semantics.

I don't like `resurrect`, since it seems hacky, but with well-defined
semantics, it's tolerable.

## Goals

The goal for this is a MVP stack-based microprocessor, with:
 - A low-level concatenative language and compiler (backend done, frontend in progress)
 - A virtual machine (done, poorly)
 - A hardware FPGA implementation (in progress)

Maybe I'll add a [Sail ISA spec](https://www.cl.cam.ac.uk/~pes20/sail/) sometime, too

## Can it do all the things?

No! It's only got 256 instruction bytes, since all jumps are 8-bit. And even if
it were unbounded, it still wouldn't be Turing complete. It's equivalent in
computability class to a push-down automaton.

## Can it do *some* of the things?

Yes! Here's an example program (`test.sm`):

```
macro neg { not 1 + }
macro sub { neg + }

// Conditions are true if negative, so less than is just subtraction
macro lt { - } // - is an alias for sub
macro gt { swap lt }
macro lte { 1 + lt }
macro gte { 1 - gt }

def upcase : ( int -- int ) {
    dup 'a' gte {
        dup 'z' lte {
            32 -
        } if
    } if
}

def main : ( -- ) {
    'h' upcase send
    'i' upcase send
    '\n' upcase send
}
```

Compile and run in the VM:
```shell
$ cd compiler ; cabal new-run compiler startup.s test.sm > /dev/null
$ # generated out.bin
$ cd ../vm ; cargo run -- ../compiler/out.bin
HI
```
