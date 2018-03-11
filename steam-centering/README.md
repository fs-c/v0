# steam-centering

Input text, get back text with added spacing to make it appear centered in steam 
profile descriptions or custom info boxes.

This is still somewhat inaccurate because space characters are weird.

### Installation

```
$ npm i -g steam-centering
```

You can now use the `steam-center` command globally.

### Flags

- `-d` 'double' - add padding on both sides of the raw line. Might cause issues 
in some cases. Allows you to easily check the precision.
- `-s` silent - no console output.
- `-c` copy - copy the line to clipboard, note that this will not work with multiple 
lines (e.G. when using `--in`).
- `--in <path>` input - readable, utf8 file with lines that should be centered, 
seperated by `\n`s. Note that lines are trimmed, so any spaces on the sides will 
be removed. To be used with `--out`.
- `--out <path>` output - path to the file that any output should be written to.
 This will *add to* files, never overwrite. File will be created if it doesn't 
exist.
- `--line <string>` line - the text that you want to be centered. Not to be 
combined with `--in`.

If neither `--in` nor `--line` are specified, lines will be read from the 
terminal. Just start typing after running, press enter to submit.

### Example

```
steam-center -d -s --in in.txt --out out.txt
```
For every line in in.txt (`--in`), adds a line in out.txt (`--out`) with 
added padding to both sides (`-d`) without any logging (`-s`).