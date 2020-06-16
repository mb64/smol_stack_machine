# Instruction set

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
0x08    resurrect   ( -- a ) [See graveyard.md for specific semantics]
0x09    imm         ( -- x ) [single-byte immediate follows the instruction]
others  nop         ( -- )
```

I say there's 10½ instructions because "everything else doesn't do anything"
only partially counts as another instruction.

Since there's not many of them, I might as well describe some in more detail:

## `send`

Outputs the value.  This is intentionally vague, since I'm not quite sure what
kind of output I'll be able to use on an FPGA.  In the VM, it write it to stdout.

## `jmp`

An unconditional jump.  Unfortunately, since the address comes from the stack,
this limits it to 8-bit addresses, and hence to only 256 bytes of program
memory. (Don't write any big programs, you won't be able to run them.)

## `skip`

A conditional skip.  This conditionally skips the next byte, depending on the
sign bit of the popped value. It skips the byte if the sign bit is 1.

*Note:* it skips bytes, not instructions. This simplifies implementation, but
be aware of this if you want to skip an `imm` (which would still be OK as long
as your immediate is at least 10).
