const fs = require('fs');
const path = require('path');
const { URL } = require('url');
const { get } = require('./request');
const { parse } = require('./parse');
const { error, warn, debug } = require('./log')();

const args = process.argv.splice(2).reduce((acc, cur, i, arr) => {
  cur.includes('-') ? acc[cur] = true
    : acc[arr[i - 1]] = cur;
  return acc;
}, {  });

// The absolute path to the output directory.
const out = path.resolve(args['--out']);
// A URL object of the page to save.
const target = new URL(args['--target']);

/**
 * Execute all Promises in an array concurrently and return the number of
 * successes and failures
 */
const executeAll = (promises) => new Promise((resolve, reject) => {
  let errors = [];
  let resolved = 0;

  function cb(err) {
    if (err) { errors.push(err) } else resolved++;

    if ((resolved + errors.length) === promises.length) {
      resolve({ resolved, errors });
    }
  }

  for (const promise of promises) {
    promise.then(cb).catch(cb);
  }
});

/**
 * Pipe an origin stream into an internally created read stream pointed at
 * the given absolute path. Missing path segments will be created.
 */
const pipeToFile = (origin, destination) => {
  path.parse(destination).dir.split(path.sep)
    .map((e, i, a) => path.join(path.sep, ...a.slice(0, a.length - i)))
    .reverse().forEach((sub) => !fs.existsSync(sub) && fs.mkdirSync(sub));

  origin.pipe(fs.createWriteStream(destination));
}

const processLink = async (node) => {
  const { attrs } = node;

  if (!attrs.href) { return; }
  
  const url = new URL(attrs.href, target);

  debug('processing link node %o', url.href);

  if (attrs.rel.includes('icon') || attrs.rel.includes('stylesheet')) {
    const dest = path.join(out, url.pathname);

    debug('piping from %o to %o',
      url.hostname + url.pathname, path.relative(__dirname, dest));

    pipeToFile(await get(url, { stream: true }), dest);
  }
}

const processPage = async (html) => {
  const document = parse(html);

  const links = [];

  for (const link of document.getNode('head').getNodes('link')) {
    delete link.parentNode;
    delete link.namespaceURI;

    links.push(processLink(link));
  }

  const { resolved, errors } = await executeAll(links);

  debug('successfully processed %o links, failed to process %o links',
    resolved, errors.length);

  if (errors.length) {
    warn('failed to process %o links', errors.length);

    errors.forEach(debug);
  }
};

(async () => {
  await processPage(await get(target));
})().catch(error);