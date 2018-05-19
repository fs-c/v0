const { join } = require('path');
const { homedir } = require('os');
const { log } = require('@sturmwalzer/logger');

const args = require('minimist')(process.argv.slice(2));
const alias = args.a || args.account || 'default';

const account = require(
  args.accounts || join(homedir(), '.steam.json')
)[alias];

const games = (
  args.games ? (
    typeof args.games === 'string'
      ? args.games.split(',') : args.games
  ) : args.config ? (
    require(join(homedir(), '.idler.json'))[alias].games
  ) : []
);

const client = new (require('steam-user'))();

client.setOption('promptSteamGuardCode', false);

client.logOn(account);

client.on('loggedOn', (details) => {
  log('logged on (%o)', details.eresult);

  log('setting games played to %o', games);  

  client.setPersona(1);
  client.gamesPlayed(games);
});

client.on('webSession', () => {
  log('negotiated web session');
});

client.on('steamGuard', (domain, cb) => {
  log('steam guard required');

  if (domain || !account.shasec) {
    throw new Error('Steam Guard error');
  }

  cb(require('steam-totp').getAuthCode(account.shasec));
});

let relogTimer;

client.on('error', (err) => {
  log('error: %o (%o)', err.message, err.enum);

  client.logOff();  

  if (relogTimer) { clearTimeout(relogTimer); }  

  relogTimer = setTimeout(
    client.logOn, 60 * 60 * 1000, account
  );
});