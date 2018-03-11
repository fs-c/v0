const fs = require('fs');
const debug = require('debug')('center');
const stringWidth = require('string-pixel-width');
const args = require('minimist')(process.argv.slice(2));

// Ex.: '0041' -> 'A'
const utfToChar = (code) => String.fromCharCode(parseInt(code, 16));
// stringWidth wrapper with fitting default size.
const getWidth = (string, size) => 
  stringWidth(string, { size: size || args.fontSize || 13 });

// Steam info box and profile summary max widths.
// 1920x1080 with default client font and font size.
const infoWidth = args.infoWidth || 600;
const summaryWidth = args.summaryWidth || 454;

const center = (line, options = args || {}) => {
  const length = getWidth(line);
  // Default to infoWidth.
  const width = (options.target && options.target === 'summary')
    ? summaryWidth
    : infoWidth

  if (length >= width) {
    console.log('line too wide');
    return;
  }

  // TODO: Steam doesn't use default Arial sizes for these space characters, 
  //       get more exact widths somehow.
  const spaces = (options.spaces || [{
    width: 1,
    code: '200A',
  }, {
    width: 2,
    code: '2009',
  }, {
    width: 13,
    code: '2003',
  }]).sort((a, b) => b.width - a.width); // Sort by width, descending.

  let remaining = (width / 2) - (length / 2);

  let block = '';
  let current = 0;

  // Don't use for..of because the order needs to be guaranteed.
  for (let i = 0; i < spaces.length; i++) {
    const space = spaces[i];

    const amount = Math.floor(remaining / space.width);

    debug('space: %o (%o), amount: %o, remaining: %o',
      space.code, space.width, amount, remaining);

    for (let i = 0; i <= amount; i++) {
      if ((remaining - space.width) < 0) { break; }

      remaining -= space.width;
      block += utfToChar(space.code);
    }
  }

  return block + line + (options.d ? block : '');
}

// TODO: Naming.
const finished = (result) => {
  if (!args.s) { console.log(`centered line: '${result}'`); }
  if (args.c) { require('copy-paste').copy(result); }
  if (args.out && args.out.length) {
    require('fs').appendFileSync(args.out, result + '\n');
  }
}

if (args.line && args.line.length) {
  finished(center(args.line));
} else if (args.file && args.file.length && fs.existsSync(args.file)) {
  const lines = fs.readFileSync(args.file, 'utf8');
  lines.split('\n').forEach((line) => finished(center(line.trim())));
} else {
  const rl = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
  });

  rl.on('line', (line) => {
    finished(center(line));
  });
}
