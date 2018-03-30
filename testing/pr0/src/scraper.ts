import { join } from 'path';
import * as debug from 'debug';
import * as minimist from 'minimist';
import { Pr0grammAPI, ItemFlags } from 'pr0gramm-api';
import { readFileSync, writeFileSync, existsSync } from 'fs';

const args = minimist(process.argv.slice(2));

if (!args.s) { debug.enable('scraper'); }

const path = args.p || './data/';
const interval = (args.i || 30) * 1000;

const log = debug('scraper');
const api = Pr0grammAPI.createWithCookies();

const { items } = JSON.parse(
  readFileSync(join(path, 'items.json'), 'utf8')
);

log(`working through %o posts`, items.length);

get(0);

async function get(index: number) {
  const bare = items[index];
  const name = join(path, `${bare.id}.json`)

  if (existsSync(join(path, name))) {
    log(`already got item info for %o, (%o/%o)`,
      bare.id, index, items.length);

    setTimeout(get, interval, index + 1);
    return;  
  }

  const item = await api.items.getInfo(bare.id);

  log('got item info for %o (%o/%o)', bare.id, index, items.length);

  writeFileSync(name, JSON.stringify(item));

  log('wrote file %o', name);

  setTimeout(get, interval, index + 1);
}