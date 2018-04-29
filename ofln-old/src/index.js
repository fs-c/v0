#!/usr/bin/env node

const { URL } = require('url');
const { get } = require('./request');
const assert = require('assert').strict;
const { debug, warn } = require('./log');
const { mkdirSync, existsSync, writeFileSync } = require('fs');
const { join, resolve, relative, parse } = require('path');

const cleanArg = (raw) => raw.split('').filter((e) => e !== '-').join('');
const parseArgs = (list) => list.reduce((acc, cur, i, arr) => {
  cur.includes('-') ? acc[cleanArg(cur)] = true
    : acc[cleanArg(arr[i - 1])] = cur;
  return acc;
}, {  });

const args = parseArgs(process.argv.splice(2));

assert(args.out, '--out must be provided');
assert(args.target, 'a --target must be provided');

const out = resolve(args.out);
const target = new URL(args.target);

const deepWriteFile = (path, file) => {
  const { dir, base } = parse(path);

  debug('writing file %o to %o', base, dir);

  relative(resolve(), dir).split('/').forEach((el, i, arr) => {
    const seg = join(...arr.slice(0, i + 1));
    !existsSync(seg) && mkdirSync(seg);
  });

  writeFileSync(path, file);
};

// const processPage = async (html) => {
//   const regexp = /href=(["'])(.*?)\1/g;
//   const matches = Array.from(new Set(html.match(regexp)));

//   for (const match of matches) {
//     const href = match.split('"')[1];
//     const url = new URL(href, target);
//     const { ext } = parse(url.pathname);

//     if (!ext || pageExtensions.includes(ext)) {
//       processPage(await get(url));
//     }

//     const file = await get(url);
//     const path = join(out, url.hostname, url.pathname);    
//     deepWriteFile(path, file);

//     const reference = relative(out, path);
//     html = html.replace(href, reference);
//     debug('replaced %o with %o', href, reference);
//   }

//   writeFileSync(join(out, 'index.html'), html);
// };

const processPage = async (html) => {
  const resources = Array.from(new Set(html.match(
    /(?<=(href|src)=(["'"]))(.*?)(?=(["']))/g
  )));

  const links = Array.from(new Set(html.match(
    /(?<=<a(.*?)href=(["'"]))(.*?)(?=(["']))/g
  )));
};

(async () => {
  processPage(await get(target));
})().catch(warn);