// TODO: Replace commander with something (even) lighter.
const args = require('commander')
  .version(require('./package.json').version)
  .option('-k, --key <key>', 'set api key')
  .option('-c, --config <path>', 'set path to config file')
  .parse(process.argv);

const path = args.config
  || require('path').join(require('os').homedir(), '.fspace.json');

const key = args.key ||
  require('fs').existsSync(path) ? require(path).key : undefined;

if (!key) {
  throw new Error('A key is required');
}

require('node-fetch')(`https://fsoc.space/api/getCodes?key=${key}`)
  .then((res) => res.json())
  .then((codes) => {
    for (const alias in codes) {
      console.log((alias + ': ').padEnd(15) + codes[alias]);
    }
  }).catch((err) => console.log(err.message));
