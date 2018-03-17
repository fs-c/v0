import { join } from 'path';
import { homedir } from 'os';
import { readFile } from 'fs';

export default {
  level: -1,
  name: 'getAccounts',
  function: getAccounts,
  description: 'Returns all accounts and their data.',
};

function getAccounts() {
  return new Promise((resolve, reject) => {
    readFile(join(homedir(), '.steam.json'), 'utf8', (err, data) => {
      if (err) { return reject(err); }
      return resolve(data);
    });
  });
}
