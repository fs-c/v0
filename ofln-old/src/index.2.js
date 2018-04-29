#!/usr/bin/env node

const { join } = require('path');
const { debug } = require('./log');
const { get } = require('./request');
const assert = require('assert').strict;
const { writeFileSync, mkdirSync } = require('fs');

const args = process.argv.splice(2);
const out = args[args.indexOf('--out') + 1];
const domain = args[args.indexOf('--domain') + 1];
const url = 'https://' + domain;

assert(out, '--out must be provided');
assert(domain, '--domain must be provided');

const dir = join(require('process').cwd(), out, domain);

mkdirSync(dir);

const pageTypes = [ 'html', 'htm' ];
const resourceTypes = [ 'ico', 'css' ];

const processPage = async (html) => {
  const regexp = /href=(["'])(.*?)\1/g;
  const matches = Array.from(new Set(html.match(regexp)));

  // Only loop over matches once for O(n) performance.
  for (const match of matches) {
    const link = match.split('"')[1];

    if (!link || !link.includes('/')) { continue; }

    const type = link.split('.').reverse()[0];
 
    debug('checking link %o of type %o', link, type);

    if (resourceTypes.includes(type)) {
      if (/https?:\/\//.test(link)) {
        const url = link.split('://')[1];
        const path = join(dir, url);

        debug('writing file from %o to %o', url, path);
        writeFileSync(path, await get(link));

        debug('replacing instances of %o in html', link);
        html.replace(link, path);
      } else {
        const path = join(dir, link);
        const subDirs = link.substr(2).split('/')
          .filter((e) => !e.includes('.'));

        debug(subDirs);

        for (let i = 1; i <= subDirs.length; i++) {
          debug(join(dir, ...subDirs.slice(0, i)));
          mkdirSync(join(dir, ...subDirs.slice(0, i)));
        }

        debug('writing file from %o to %o', link, path);
        writeFileSync(path, await get(url + link.substr(1)));
      }
    }

    if (!link.includes('.') || pageTypes.includes(type)) {
      /* Handle page if recursive mode. */
    }

    writeFileSync(join(dir, 'index.html'), html);
  }
};

(async () => {
  processPage(await get(url));
})();
