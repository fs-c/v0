import * as debug from 'debug';
import { readFileSync, writeFileSync } from 'fs';
import { Pr0grammAPI, ItemFlags } from 'pr0gramm-api';

debug.enable('scraper');
const log = debug('scraper');

const interval = 30 * 1000;
const api = Pr0grammAPI.createWithCookies();

const items = JSON.parse(readFileSync('data/items.json', 'utf8')).items;

log(`working through %o posts`, items.length);

get(0);

async function get(index: number) {
  const bare = items[index];
  const item = await api.items.getInfo(bare.id);

  log('got item info for %o (%o)', bare.id, index);

  const name = `data/${bare.id}.json`
  writeFileSync(name, JSON.stringify(item));

  log('wrote file %o', name);

  setTimeout(get, interval, index + 1);
}