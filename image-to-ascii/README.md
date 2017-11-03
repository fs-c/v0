## Another image to ASCII converter

Does not have any non-JS dependencies. No need to grapple with node-gyp errors,
missing python or C dependencies, just do `npm i` and you're good to go.

The only exported function takes a path to a file (JPG or PNG) and optionally an
options with one or more of the following properties:

- `color` - boolean; whether or not characters should be colorized.
- `chars` - as a string, the characters to use, from lightest to darkest.
- `cRatio` - a monospace character is taller than it is wide, this defines the integer approximation of the ratio of the width to height.
- `width` and `height` - the width and height of the output, if only one is provided, the other will be scaled accordingly.
