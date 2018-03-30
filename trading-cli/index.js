#!/usr/bin/env node

const moment = require('moment');
const program = require('commander');

const { Trader } = require('./src/Trader');

program
  .option('-c, --config <path>',
    'The path to the accounts file.', '~/.steam.json')
  .option('-a, --account <alias>', 'The steam account to use.', 'default')

const path = program.config.replace('~', require('os').homedir());
const account = require('fs').existsSync(path)
  ? require(path)[program.account]
  : null;

if (!account) {
  console.log('Missing account.');
  process.exit(1);
}

const trader = new Trader(account);

const logError = (boom) => {
  console.log(`error: ${boom.message || boom.err.message}`);
}

trader.on('clientError', logError);
trader.on('managerError', logError);

program.command('list')
  .alias('ls')
  .description('List all active trade offers')
  .action(listOffers);

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
    console.log(`   Offer message: ${offer.message}.`);    
    console.log('')
  }
}

program.command('monitor')
  .alias('monit')
  .description('Watch for changes on any trades related to the account.')
  .action(monitorEvents);

async function monitorEvents() {
  const logChange = (offer, oldState) => {
    const olds = Trader.ETradeOfferState[oldState];
    const news = Trader.ETradeOfferState[offer.state];
  
    console.log(`offer ${offer.id} changed from ${olds} to ${news}`);
  }

  await trader.initialise();

  trader.on('ready', () => console.log('ready'));
  
  trader.on('newOffer', (offer) => {
    console.log(
      `new offer (${offer.id} - ${offer.state}) by ${offer.partner.toString()} `
      + `with message: ${offer.message}.`
    )
  });

  trader.on('sentOfferChanged', logChange);
  trader.on('receivedOfferChanged', logChange);

  trader.on('sentOfferCanceled', (offer) => {
    console.log(`offer ${offer.id} has been canceled`)
  });

  trader.on('pollFailure', (err) => {
    console.log(`failed polling: ${err.message}`);
  });
}

program.command('accept <offer>')
  .description('Accepts the given offer. You may only provide a part of the ID'
    + ', the closest match will be chosen.')
  .action(acceptOffer);

async function acceptOffer(partial) {
  await trader.initialise();

  const offer = await trader.getOffer(partial);

  // Accept offer, now that we have it...
}

program.version(require('./package.json').version);
program.parse(process.argv);