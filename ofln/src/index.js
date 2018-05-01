const { URL } = require('url');
const { get } = require('./request');
const { parse } = require('./parse');
const { join, resolve, relative } = require('path');
const { error, warn, debug } = require('./log');
const { createWriteStream, existsSync, mkdirSync } = require('fs');

const args = process.argv.splice(2).reduce((acc, cur, i, arr) => {
  cur.includes('-') ? acc[cur] = true
    : acc[arr[i - 1]] = cur;
  return acc;
}, {  });

const out = resolve(args['--out']);
const target = new URL(args['--target']);

const execute = async (promises) => {
  let resolved = 0;
  let rejected = 0;

  promises.forEach((prms) => prms.then(() => resolved++).catch(rejected++));

  return [ resolved, rejected ];
}

const buildDir = (path) => {
  const subDirs = path.split('/')
    .map((el, i, arr) => join(...arr.slice(0, arr.length - i)))
    .reverse();

  debug(subDirs);

  for (const sub of subDirs) {
    if (!existsSync(sub)) {
      mkdirSync(sub);
    }
  }
}

const processLink = async (node) => {
  const { attrs } = node;
  const url = attrs.href ? new URL(attrs.href, target) : undefined;
  const path = join(out, url.hostname, url.pathname);  

  if (
    (attrs.rel.includes('icon') || attrs.rel.includes('stylesheet'))
      && attrs.href
  ) {
    buildDir(out, path);
    (await get(url, { stream: true })).pipe(createWriteStream(path));

    debug('wrote %o to %o', url.href, path);
  }
}

const processPage = async (html) => {
  const document = parse(html);

  const links = [];

  for (const link of document.getNode('head').getNodes('link')) {
    delete link.parentNode;
    delete link.namespaceURI;
    
    debug('found link');

    links.push(processLink(link));
  }

  const [ succeeded, failed ] = await execute(links);

  debug('successfully processed %o links, failed to process %o links',
    succeeded, failed);

  if (failed) {
    warn('failed to process %o links', failed);
  }
};

(async () => {
  processPage(await get(target));
})().catch(error);