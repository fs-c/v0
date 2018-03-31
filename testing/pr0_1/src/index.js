const { join } = require('path');
const { log } = require('@sturmwalzer/logger');
const { Pr0grammAPI, ItemFlags } = require('pr0gramm-api');
const { writeFileSync, readFileSync } = require('fs');

const args = require('minimist')(process.argv.slice(2));
const dataPath = args.data;

const api = Pr0grammAPI.createWithCookies();

async function get(id) {
  log('getting items newer than %o', id);

  const response = await api.items.getItemsNewer({
    newer: id,
    promoted: true,
    flags: ItemFlags.SFW,
  });

  log(response)
}

get(args.start);