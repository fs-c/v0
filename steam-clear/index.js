#!/usr/bin/env node

const rls = require('readline-sync');
const SteamUser = require('steam-user');
const steamtotp = require('steam-totp');
const debug = require('debug')('clear');
const args = require('minimist')(process.argv.slice(2));

const intervals = {
  hide: 5 * 60 * 1000,
  error: 10 * 60 * 1000,
  playing: 15 * 60 * 1000,
  ratelimit: 6 * 60 * 60 * 1000,
}

const { join } = require('path');
const path = args.a || args.accounts 
  || join(require('os').homedir(), '.steam.json')

const { existsSync } = require('fs');
let accounts = path ? (
  existsSync(path) ? require(path) : undefined
) : undefined;

if (!accounts || typeof accounts !== 'object') {
  throw new Error('No account(s) provided');
}

if (Array.isArray(accounts)) {
  accounts = accounts.reduce((acc, cur) => {
    acc[cur.accountName] = cur;
    return acc;
  }, {});
}

if (args.v || args.verbose) {
  require('debug').enable('clear');
}

const hide = (client) => {
  debug(`hiding games`);

  client.gamesPlayed([ 413851, 413857, 413859, 413856 ]);
  client.gamesPlayed();
}

const login = (client, account) => {
  debug(`logging in`);

  client.logOn(account);
}

const build = (account) => {
  debug(`building ${account.accountName}`);

  const client = new SteamUser();

  client.setOption('promptSteamGuardCode', false);

  login(client, account);

  client.on('steamGuard', (domain, callback) => {
    debug(`steamGuard received`);
    steamtotp.getAuthCode(account.shasec, (err, code, offset, latency) => {
      if (err) console.error(err)
      callback(code)
    });
  });

  let timer;
  client.on('loggedOn', details => {
    debug(`logged on`);
    hide(client);
    timer = setInterval(hide, intervals.hide, client);
  });

  client.on('error', err => {
    clearInterval(timer);

    debug('error: ' + err.message || err.msg || err);

    if (err.message === 'LoggedInElsewhere') {
      debug(`retrying in ${intervals.playing}ms.`);

      setTimeout(
        function () {
          debug(`timer restart`);
          timer = setInterval(hide, intervals.hide, client);
        },
        intervals.playing
      );
    } else {
      debug(`logging off, restart in`
        + `${intervals.ratelimit}/${intervals.error}ms`);

      client.logOff();

      const interval = (err.message === 'RateLimitExceeded' 
        ? intervals.ratelimit 
        : intervals.error);

      setTimeout(login, interval, client);
    }
  })
}

for (const name in accounts) {
  const hide = rls.keyInYN(`Clear activity for account ${name}?`);

  if (hide) {
    build(accounts[name]);
  }
}
