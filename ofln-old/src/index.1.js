#!/usr/bin/env node

const { join } = require('path');
const { warn } = require('./log');
const { mkdirSync, writeFileSync } = require('fs');
const { get } = require('./request');

const buildDir = (absolute, path) => path.split('/').forEach(
  (el, i, arr) => mkdirSync(join(absolute, ...arr.slice(0, i + 1)))
);

const argv = process.argv.splice(2);
const getArgs = (keys, args) =>
  keys.map((key) => args[args.indexOf(key) + 1]);

const [
  outDir, base, secure = true,
] = getArgs([ '--out', '--base', '--secure' ], argv);

const out = join(__dirname, outDir);

const pageTypes = [ 'html', 'htm' ];
const resourceTypes = [ 'ico', 'css' ];

const processPage = async (html) => {
  const regexp = /href=(["'])(.*?)\1/g;
  const matches = Array.from(new Set(html.match(regexp)));

  for (const match of matches) {
    const uri = match.split('"')[1];
    debug('found uri %o', uri);

    if (!uri) { continue; }

    const type = uri.split('.').reverse()[0];
    
    const path = join(out, /https?:\/\//.test(uri) ? (
      join(...uri.split('://')[1].split('/'))
    ) : uri);

    debug('writing %o to %o', uri, path);
    writeFileSync(path, await get())
  }
};

(async () => {
  buildDir(out);
  processPage(await get('http' + secure ? 's' : '' + '://' + base));
})().catch(warn);