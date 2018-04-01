const { join } = require('path');
const { log } = require('@sturmwalzer/logger');
const { writeFileSync, readFileSync } = require('fs');
const { Pr0grammAPI, ItemFlags } = require('pr0gramm-api');

const args = require('minimist')(process.argv.slice(2));
const dataPath = args.data;

const api = Pr0grammAPI.createWithCookies();

const capFirst = (string) => {
  return string.charAt(0).toUpperCase() + string.slice(1);
}

async function get() {
  const response = await api.items['getItems' + (
    args.newer ? 'Newer' : 
    args.older ? 'Older' : 
    args.around ? 'Around' : ''
  )]({
    newer: args.newer,
    older: args.older,
    around: args.around,

    promoted: true,
    flags: ItemFlags.SFW,
  });

  if (response.error) {
    log('something went wrong: %o', response.error);
  }

  const { items } = response;

  log('got %o items', items.length);

  writeFileSync('res.json', JSON.stringify(
    items.sort((a, b) => a.ts - b.ts)
  ));
}

get();