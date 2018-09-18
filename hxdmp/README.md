# `h`e`xd`u`mp`

Recently, while crawling through a dump of some memory, it occured to me that I never tried to implement a `hexdump` utility.

Do I need more justification than that?

So, inspired by `xxd`, any `hxdmp` version shall implement the following:

Given an input stream, create and print out a hex dump of it that is readable. Yeah, I'm not going to get more specific than that. In practice, I mostly just copied the default `xxd` look since I feel like that's a rather comfortable view. Maybe I'll explore different avenues in the future though, who knows.

```
00000000: 2369 6e63 6c75 6465 203c 7374 6469 6f2e  #include.<stdio.
00000010: 683e 0a23 696e 636c 7564 6520 3c75 6e69  h>.#include.<uni
00000020: 7374 642e 683e 0a0a 696e 7420 6d61 696e  std.h>..int.main
00000030: 2829 0a7b 0a09 636f 6e73 7420 696e 7420  ().{..const.int.
00000040: 636f 6c73 203d 2038 3b0a 0963 6f6e 7374  cols.=.8;..const
00000050: 2069 6e74 2063 6f6c 5f73 697a 6520 3d20  .int.col_size.=.
00000060: 323b 0a09 636f 6e73 7420 696e 7420 6368  2;..const.int.ch
00000070: 756e 6b5f 7369 7a65 203d 2063 6f6c 7320  unk_size.=.cols.
00000080: 2a20 636f 6c5f 7369 7a65 3b0a 0a09 696e  *.col_size;...in
...
00000260: 097d 0a7d                                .}.}
```

### Installation

- `hexdump.js`, [NodeJS](https://nodejs.org)
- `hexdump.d` any [D](https://dlang.org) compiler should do
- `hexdump.c` should compile just about anywhere, as long as you have a C99 compliant compiler