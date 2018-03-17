require('dotenv').config();

const INTERVAL_HIDE = 5 * 60 * 1000;
const INTERVAL_ERROR = 10 * 60 * 1000;
const INTERVAL_PLAYING = 15 * 60 * 1000;
const INTERVAL_RATELIMIT = 6 * 60 * 60 * 1000;

const fs = require('fs');
const { join } = require('path');
const rls = require('readline-sync');
const SteamUser = require('steam-user');
const steamtotp = require('steam-totp');
const debug = require('debug')('clear');

const home = process.cwd();
const path = process.env.ACCOUNTS;

let accounts = path ? (
  fs.existsSync(path) 
    ? require(path) 
    : fs.existsSync(join(home, path))
      ? require(join(home, path))
      : undefined
) : (
  fs.existsSync(join(home, './steam.json'))
    ? require(join(home, './steam.json'))
    : undefined
);

if (!accounts || typeof accounts !== 'object') {
  throw new Error('Accounts not defined or invalid');
}

if (Array.isArray(accounts)) {
  accounts = accounts.reduce((acc, cur) => {
    acc[cur.accountName] = cur;
    return acc;
  }, {});
}

const hide = (client) => {
  log.verbose(`hiding games`);

  client.gamesPlayed([ 399220, 399080, 399480 ]);
  client.gamesPlayed();
}

const login = (client, account) => {
  log.verbose(`logging in`);

  client.logOn({
    accountName: account.name,
    password: account.password
  });
}

const build = (account) => {
  log.verbose(`building ${account.name}`);

  const client = new SteamUser();

  client.setOption('promptSteamGuardCode', false);

  login(client, account);

  client.on('steamGuard', (domain, callback) => {
    log.debug(`steamGuard received`);
    steamtotp.getAuthCode(account.shasec, (err, code, offset, latency) => {
      if (err) console.error(err)
      callback(code)
    });
  });

  let timer;
  client.on('loggedOn', details => {
    log.info(`logged on`);
    hide(client);
    timer = setInterval(hide, INTERVAL_HIDE, client);
  });

  client.on('error', err => {
    clearInterval(timer);

    log.error('error: ' + err.message || err.msg || err);

    if (err.message === 'LoggedInElsewhere') {
      log.debug(`retrying in ${INTERVAL_PLAYING}ms.`);

      setTimeout(
        function () {
          log.debug(`timer restart`);
          timer = setInterval(hide, INTERVAL_HIDE, client);
        },
        INTERVAL_PLAYING
      );
    } else {
      log.debug(`logging off, restart in`
        + `${INTERVAL_RATELIMIT}/${INTERVAL_ERROR}ms`);

      client.logOff();

      const interval = (err.message === 'RateLimitExceeded' 
        ? INTERVAL_RATELIMIT 
        : INTERVAL_ERROR);

      setTimeout(login, interval, client);
    }
  })
}

for (const name in accounts) {
  const hide = rls.keyInYN(
    `Clear activity for account ${name}?`);

  if (hide) {
    build(accounts[name]);
  }
}
