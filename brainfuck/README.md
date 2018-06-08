## brainfuck

A [brainfuck](http://www.muppetlabs.com/~breadbox/bf/) interpreter written in JS. Ironic, really.

Implementation details:
- All cells hold 64 bit floating point integers (largest: 2^53 - 1) which may be negative.
- Characters are represented by their UTF16 code.
- The pointer may not be smaller than zero.
- If input is expected but stdin empty, the program will block and wait for input.