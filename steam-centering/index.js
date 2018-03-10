const stringWidth = require('string-pixel-width');
const args = require('minimist')(process.argv.slice(2));
const rl = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});

// Ex.: '0041' -> 'A'
const utfToChar = (code) => String.fromCharCode(parseInt(code, 16));
// stringWidth wrapper with fitting default size.
const getWidth = (string, size) => 
  stringWidth(string, { size: size || args.fontSize || 13 });

const center = (line, options = {}) => {
  // Steam info box and profile summary max widths.
  // 1920x1080 with default client font and font size.
  const infoWidth = options.infoWidth || 600;
  const summaryWidth = options.summaryWidth || 454;

  const length = getWidth(line);
  // Default to infoWidth.
  const width = (options.target && options.target === 'summary')
    ? summaryWidth
    : infoWidth

  if (length >= width) {
    console.log('line too wide')
    return;
  }

  const spaces = (options.spaces || [{
    width: 3.08,
    code: '200A',
  }, {
    width: 4.92,
    code: '2009',
  }, {
    width: 15,
    code: '2003',
  }]).sort((a, b) => a.width - b.width); // Sort by width, descending.

  let remaining = (width / 2) - (length / 2);

  let block = '';
  let current = 0;

  // Don't use for..of because the order needs to be guaranteed.
  for (let i = 0; i < spaces.length; i++) {
    const space = spaces[i];

    const amount = Math.floor(remaining / space.width);
    console.log(amount, remaining, space);
    for (let i = 0; i <= amount; i++) {
      if ((remaining - space.width) < 0) { break; }

      remaining -= space.width;
      block += utfToChar(space.code);
    }
  }

  return block + line;
}

rl.on('line', (line) => {
  console.log(`line width: ${getWidth(line)}`);

  const result = center(line, args)
  console.log(`centered line: '${result}'`);

  require('fs').appendFileSync('out.txt', result + '\n');
});
