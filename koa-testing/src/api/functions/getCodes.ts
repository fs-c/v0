import { join } from 'path';
import { homedir } from 'os';
import { readFile } from 'fs';
import { getAuthCode } from 'steam-totp';

export default {
  level: 0,
  name: 'getCodes',
  function: getCodes,
  description: 'Returns the steam mobile 2FA code for all accounts with a shared secret.',
};

function getCodes() {
  return new Promise((resolve, reject) => {
    readFile(join(homedir(), '.steam.json'), 'utf8', (err, data) => {
      if (err || !data) { return reject(err || new Error('No data.')); }

      const codes: any = {};
      const accounts = JSON.parse(data);

      for (const name of Object.keys(accounts)) {
        const account = accounts[name];

        if (account && account.shasec) {
          codes[name] = getAuthCode(account.shasec);
        }
      }

      return resolve(codes);
    });
  });
}
