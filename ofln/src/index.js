#!/usr/bin/env node

const { debug } = require('./log');
// const assert = require('assert').strict;

// const args = process.argv.slice(2);
// const out = args[args.indexOf('--out') + 1];

// assert(out, '--out must be given');

const handle = (chunk) => {
  const match = /href="(.*?)"/g.exec(chunk);

  debug(match[1]);
};

const stream = process.stdin;

let previous = undefined;
stream.on('data', (chunk) => {
  if (previous === undefined) {
    previous = chunk;
  } else {
    handle(previous + chunk);
    previous = undefined;
  }
});
