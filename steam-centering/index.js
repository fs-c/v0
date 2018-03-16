const program = require('commander')
  .version(require('./package.json').version)
  .option('-I, --in <path>', 'file to read from (to be used with --out)')
  .option('-O, --out <path>', 'file to write to (to be used with --in)')
  .option('-L, --line <line>', 'string to center')
  .option('-t, --target <target>', 'type of box to center for',
    /^(summary|infobox)$/i, 'infobox')
  .option('-d, --double', 'add padding on both sides')
  .option('-c, --copy', 'add the centered string to clipboard')
  .option('-s, --silent', 'be silent')  
  .parse(process.argv);

// Ex.: '0041' -> 'A'
const utfToChar = (code) => String.fromCharCode(parseInt(code, 16));

// stringWidth wrapper with fitting default font size.
const stringWidth = require('string-pixel-width');
const getLength = (string, size = 13) => 
  stringWidth(string, { size });

// Steam info box and profile summary max widths.
// 1920x1080 with default client font and font size.
const widths = {
  infobox: 454,
  summary: 600,
};

// TODO: Steam doesn't use default Arial sizes for these space characters, 
//       get more exact widths somehow.
const spaces = [{
  width: 1,
  code: '200A',
}, {
  width: 2,
  code: '2009',
}, {
  width: 13,
  code: '2003',
}].sort((a, b) => b.width - a.width);

const center = (line, options) => {
  const length = getLength(line);

  if (length > options.width) {
    throw new Error('Line too wide');
  }

  let block = '';
  let remaining = (options.width / 2) - (length / 2);

  // Don't use for..of because the order needs to be guaranteed.
  for (let i = 0; i < spaces.length; i++) {
    const space = spaces[i];
    const amount = Math.floor(remaining / space.width);

    for (let i = 0; i <= amount; i++) {
      if ((remaining - space.width) < 0) { break; }

      remaining -= space.width;
      block += utfToChar(space.code);
    }
  }

  return block + line + (options.double ? block : '');  
};

const { existsSync, readFileSync, appendFileSync } = require('fs');

const lines = (program.in && existsSync(program.in)) ? (
  (readFileSync(program.in, 'utf8')).split('\n').map((e) => e.trim())
) : [ program.line ];

if (!lines[0]) { process.exit(1); }

for (const line of lines) {
  const centered = center(line, { width: widths[program.target] });

  if (!program.silent) { console.log(centered); }  
  if (program.copy) { require('copy-paste').copy(centered); }
  if (program.out) { appendFileSync(program.out, centered + '\n'); }
}
