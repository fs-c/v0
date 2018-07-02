Cheating in osu!mania.

Usage: `usage: <executable> -m <map path> -p <osu! pid>`, compile with `gcc *.c -lX11 -lXtst`.

This _doesn't work_, currently. I'm fairly certain the logic is solid, but it segfaults once it tries to read the current songs playback time when in a map - which works fine while in the menu.

Since Wine reports
```
0009:fixme:crypt:SystemFunction041 (0x20c5dc, 20, 0): stub [RtlDecryptMemory]
0009:fixme:crypt:SystemFunction040 (0x20c5dc, 20, 0): stub [RtlEncryptMemory]
```
this might be due to memory encryption (which I doubt).