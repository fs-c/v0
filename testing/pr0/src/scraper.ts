import { join } from 'path';
import * as debug from 'debug';
import * as minimist from 'minimist';
import { readFileSync, writeFileSync, existsSync } from 'fs';
import {
  Item,
  ItemFlags,
  Pr0grammAPI,
} from 'pr0gramm-api';

(async () => {

const args = minimist(process.argv.slice(2));

if (!args.s) { debug.enable('scraper'); }

const path = args.p || './data/';
const interval = (args.i || 30) * 1000;

const log = debug('scraper');
const api = Pr0grammAPI.createWithCookies();

const getItems = async (): Promise<Item[]> => {
  if (args.r) {
    log('reading items from disk');

    return JSON.parse(
      readFileSync(join(path, 'items.json'), 'utf8')
    );
  }

  const fresh = args.newer ? (
    await api.items.getItemsNewer({
      promoted: true,
      flags: ItemFlags.SFW,
      newer: args.newer as number,
    })
  ) : (
    await api.items.getItems({
      promoted: true,
      flags: ItemFlags.SFW,
    })
  );

  log('fetched new items');

  if (args.w) {
    const name = join(path, `items-${Date.now()}.json`)
    writeFileSync(name, JSON.stringify(fresh));
    log('wrote file %o', name);
  }

  if (args.n) {
    return fresh.items;
  }

  const items = Array.from(new Set(
    JSON.parse(readFileSync(join(path, 'items.json'), 'utf8'))
      .concat(fresh.items) as Item[]
  ));

  writeFileSync(join(path, 'items.json'), JSON.stringify(items));

  return items;
}

const items = await getItems();

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
  log(
    '   item created %o by %o (%o tags, %o comments - +%o/-%o)',
    bare.created, bare.user, item.tags.length, 
    item.comments.length, bare.up, bare.down
  );

  writeFileSync(name, JSON.stringify(item));

  log('wrote file %o', name);

  setTimeout(get, interval, index + 1);
}

})();