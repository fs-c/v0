const { api } = require('./pr0');
const { join } = require('path');
const { log } = require('@sturmwalzer/logger');
const { writeFileSync, readFileSync, existsSync } = require('fs');

const args = require('minimist')(process.argv.slice(2));
const dataPath = args.path;

function details(id, cb) {
  const path = join(dataPath, `item3/${id}.json`);

  if (existsSync(path)) {
    log('already got details for %o', id);
    return;
  }

  api.items.info(id)
    .then((item) => {
      writeFileSync(path, JSON.stringify(item));

      log('got details for %o and wrote %o', id, path);

      cb(null);
    }).catch((err) => {
      err.itemId = id;
      return cb(err);
    })
}

async function list(id) {
  log('getting items newer than %o', id);

  const { items } = await api.items.newer(id, { promoted: 0 });

  log('got %o items', items.length);

  const path = join(dataPath, `items-${Date.now()}.json`);
  writeFileSync(path, JSON.stringify(items));

  log('wrote %o to disk', path);

  for (const { id } of items) {
    details(id, (err) => {
      if (err) {
        log('failed to get details for %o: %o',
          id, err.message);
      }
    });
  }

  list(items[items.length - 1].id);
}

list(args.start);