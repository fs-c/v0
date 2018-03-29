#!/usr/bin/env node

const moment = require('moment');
const program = require('commander');

const { Trader } = require('./src/Trader');

program
  .option('-c, --config <path>',
    'The path to the accounts file.', '~/.steam.json')
  .option('-a, --account <alias>', 'The steam account to use.', 'default');

program.command('list')
  .alias('ls')
  .description('List all active trade offers')
  .action(listOffers);

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

async function listOffers() {
  await trader.initialise();

  const offers = await trader.getOffers();

  for (const offer of offers) {
    const sender = offer.isOurOffer ? 'you' : offer.partner.toString();
    const recipient = offer.isOurOffer ? offer.partner.toString() : 'you';

    const toGive = offer.itemsToGive.length;
    const toReceive = offer.itemsToReceive.length;

    const created = moment(offer.created).fromNow();
    const expires = moment(offer.expires).fromNow();

    console.log(`Offer ${offer.id} by ${sender} for ${recipient}.`);
    console.log(`   ${toGive} items to give, ${toReceive} items to receive.`);
    console.log(`   It was created ${created} and expires ${expires}.`);
    console.log('')
  }
}

async function monitorEvents() {

}