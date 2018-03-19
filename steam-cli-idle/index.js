const debug = require('debug')('idler');
const args = require('minimist')(process.argv.slice(2));

if (!args.silent) { require('debug').enable('idler'); }

const { join } = require('path');
const { homedir } = require('os');

const alias = args.a || args.account || 'default';

const account = require(
  args.accounts || join(homedir(), '.steam.json')
)[alias];

const games = (
  args.games ? (
    typeof args.games === 'string' ? args.games.split : args.games
  ) : args.config ? (
    require(join(homedir(), '.idler.json'))[alias].games
  ) : []
);

// .map((e) => (typeof e !== 'string' || typeof e !== 'number') ? false : e)
// .filter((e) => e !== false);

const client = new (require('steam-user'))();

client.setOption('promptSteamGuardCode', false);

client.logOn(account);

client.on('loggedOn', (details) => {
  debug('logged on (%o)', details.eresult);
});

client.on('webSession', () => {
  debug('negotiated web session');
  debug('setting games played to %o', games);

  client.setPersona(1);
  client.gamesPlayed(games);
});

client.on('steamGuard', (domain, cb) => {
  debug('steam guard required');

  if (domain || !account.shasec) {
    throw new Error('Steam Guard error');
  }

  cb(require('steam-totp').getAuthCode(account.shasec));
});

let relogTimer;

client.on('error', (err) => {
  debug('error: %o', err.message);

  client.logOff();  

  if (relogTimer) { clearTimeout(relogTimer); }  
  
  relogTimer = setTimeout(() => {
    client.logon(account);
  }, 60 * 60 * 1000);
});