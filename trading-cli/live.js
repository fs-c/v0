const moment = require('moment');
const rls = require('readline-sync');
const program = require('commander');

const { Trader } = require('./src/Trader');

program
  .option('-c, --config <path>',
    'The path to the accounts file.', '~/.steam.json')
  .option('-a, --account <alias>', 'The steam account to use.', 'default')

program.version(require('./package.json').version)
  .parse(process.env.argv);

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

  if (boom.isFatal) { process.exit(1); }
}

trader.on('clientError', logError);
trader.on('managerError', logError);

const getUser = (id) => new Promise((resolve, reject) => {
  this.trader.community.getSteamUser(id, (err, user) => {
    if (err) { resolve(null); } else resolve(user)
  })
});

const offerCallback = (err) => {
  if (err) {
    console.log(`Failed: ${err.message} (${Trader.EResult[err.eresult]})`);
  } else { console.log(`Success.`); }
}

trader.on('newOffer', async (offer) => {
  const partner = await getUser(offer.partner) || { id: offer.partner };

  const created = moment(offer.created).fromNow();
  const expires = moment(offer.expires).fromNow();

  console.log(`New offer #${offer.id} by ${partner.name || partner.id}.`);
  console.log(`It was created ${created} and expires ${expires}.`);
  console.log('');

  const toGive = offer.itemsToGive;
  const toReceive = offer.itemsToReceive;

  const highest = toGive.length > toReceive.length
    ? toGive.length : toReceive.length;
  
  console.log(`Items to give: `.padEnd(28) + `| Items to receive: `);
  for (let i = 0; i < highest; i++) {
    const giv = toGive[i] || {}, rec = toReceive[i] || {};

    console.log(`${giv.name || ''}`.padEnd(28) + `${rec.name || ''}`);
  }

  console.log('');

  if (rls.keyInYN(`Do you wish to accept offer #${offer.id}?`)) {
    offer.accept(offerCallback);
  } else {
    offer.decline(offerCallback);
  }
});
