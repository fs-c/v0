const { join } = require('path');
const { log } = require('@sturmwalzer/logger');
const { Pr0grammAPI, ItemFlags } = require('pr0gramm-api');
const { writeFileSync, readFileSync } = require('fs');

const args = require('minimist')(process.argv.slice(2));
const dataPath = args.data;

const api = Pr0grammAPI.createWithCookies();

async function get(id) {
  log('getting items around %o', id);

  const { items } = await api.items.getItemsAround({
    around: id,
    promoted: true,
    flags: ItemFlags.SFW,
  });

  log('got %o items', items.length);

  const lower = items.filter((item) => item.id == id)[0];

  log('lower bounds timestamp is %o', lower.ts);

  const relevant = items.filter((item) => item.ts >= lower.ts);

  log('%o relevant items found', relevant.length);

  const path = join(dataPath, 'items.json');
  const old = JSON.parse(readFileSync(path), 'utf8');

  log('read %o old items from %o', old.length, path);

  const complete = Object.values(
    old.concat(relevant).reduce((acc, cur) => {
      acc[cur.id] = cur;

      return acc;
    }, {})
  );

  log('merged items, new total is %o', complete.length);

  writeFileSync(path, JSON.stringify(complete));

  log('added relevant items to %o', path);

  const upper = items.sort((a, b) => b.ts - a.ts)[0];

  log('upper bounds timestamp of relevant items is %o', upper.ts);

  get(upper.id);
}

get(args.start);