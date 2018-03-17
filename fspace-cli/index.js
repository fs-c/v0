#!/usr/bin/env node

const program = require('commander');

program.version(require('./package.json').version);

program
  .option('-k, --key <key>', 'set api key')
  .option('-c, --config <path>', 
    'set path to config file', '~/.fspace.json')

const path = program.config.replace('~', require('os').homedir());
const key = program.key 
  || require('fs').existsSync(path) ? require(path).key : undefined;

program
  .command('run <function>', 'make request and output response')
  .action(run);

program
  .command('codes')
  .alias('c')
  .action(() => {
    get('getCodes').then((codes) => {
      for (const alias in codes) {
        console.log((alias + ': ').padEnd(15) + codes[alias]);
      }
    }).catch((err) => console.error(err));
  })

program.parse(process.argv);

function get(func) { return new Promise((resolve, reject) => {
  require('node-fetch')(`https://fsoc.space/api/${func}?key=${key}`)
    .then((res) => resolve(res.json())).catch(reject)
})}

async function run(func) {
  try {
    const res = await get(func);
    console.log(res);
  } catch(e) { console.error(e.message) };
}