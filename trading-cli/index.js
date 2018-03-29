#!/usr/bin/env node

const program = require('commander');

const { Trader } = require('./src/Trader');

program
  .option('-c, --config <path>',
    'The path to the accounts file.', '~/.steam.json')
  .option('-a, --account <alias>', 'The steam account to use.', 'default');

program.command('list')
  .alias('ls')
  .description('List all incoming trade requests.')
  .action(listTrades);

program.command('monitor')
  .alias('monit')
  .description('Watch for changes on any trades related to the account.')
  .action(monitorEvents);

program.version(require('./package.json').version);
program.parse(process.argv);

const path = program.config.replace('~', require('os').homedir());
const account = require('fs').existsSync(path)
  ? require(path)[program.account]
  : null;

if (!account) {
  console.log('Missing account.');
  process.exit(1);
}

const trader = new Trader(account);

async function listTrades() {
  await trader.initialise();


}

async function monitorEvents() {

}