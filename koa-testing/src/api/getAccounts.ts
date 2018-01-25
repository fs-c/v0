import { join } from 'path';
import { homedir } from 'os';
import { readFile } from 'fs';

export default {
  level: -1,
  name: 'getAccounts',
  function: getAccounts,
  description: 'Returns all accounts and their data.',
};

async function getAccounts() {
  readFile(join(homedir(), '.steam.json'), 'utf8', (err, data) => {
    if (err) { throw err; }
    return data;
  });
}
