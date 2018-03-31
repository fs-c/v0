import { join } from 'path';
import * as debug from 'debug';
import * as minimist from 'minimist';
import { Pr0grammAPI, ItemFlags } from 'pr0gramm-api';
import { readFileSync, writeFileSync, existsSync } from 'fs';

(async () => {

const args = minimist(process.argv.slice(2));

if (!args.s) { debug.enable('scraper'); }

const path = args.p || './data/';
const interval = (args.i || 30) * 1000;

const log = debug('scraper');
const api = Pr0grammAPI.createWithCookies();

const getItems = async () => {
  if (existsSync(join(path, 'items.json')) && !args.r) {
    log('reading items from disk');

    return JSON.parse(readFileSync(join(path, 'items.json'), 'utf8'));
  }

  log('fetching items');

  const items = await api.items.getItems({
    promoted: true,
    flags: ItemFlags.SFW,
  });

  writeFileSync(join(path, 'items.json'), JSON.stringify(items));

  return items;
}

const { items } = await getItems();

log('working through %o posts with an interval of %o',
  items.length, interval);

get(0);

async function get(index: number) {
  if (index >= items.length) {
    return log('done');
  }

  const bare = items[index];
  const name = join(path, 'item', `${bare.id}.json`)

  if (existsSync(name)) {
    log(`already got item info for %o, (%o/%o)`,
      bare.id, index + 1, items.length);

    get(index + 1);
    return;  
  }

  const item = await api.items.getInfo(bare.id);

  log('got item info for %o (%o/%o)', bare.id, index + 1, items.length);

  writeFileSync(name, JSON.stringify(item));

  log('wrote file %o', name);

  setTimeout(get, interval, index + 1);
}

})();