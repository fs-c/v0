# steam-centering

Input text, get back text with added spacing to make it appear centered in steam profile descriptions or custom info boxes.

This is still somewhat inaccurate because steam seems to do funny things with whitespace characters.

## Usage

Installation, 
```
$ npm i -g steam-centering
```
...and a usage example: 
```
steam-center -ds -I in.txt -O out.txt --target summary
```
Will read all lines from in.txt (`-I`), add padding on both sides of every line (`-d`) to make it appear centered in steam profile summaries (`--target`). It will write the centered lines to out.txt (`-O`) and not output anything in the terminal (`-s`).

## Options

    -V, --version          output the version number
    -I, --in <path>        file to read from (to be used with --out)
    -O, --out <path>       file to write to (to be used with --in)
    -L, --line <line>      string to center
    -t, --target <target>  type of box to center for (default: infobox)
    -d, --double           add padding on both sides
    -c, --copy             add the centered string to clipboard
    -s, --silent           be silent
    -h, --help             output usage information