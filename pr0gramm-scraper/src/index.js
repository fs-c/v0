const { api } = require('./pr0');
const { join } = require('path');
// const Progress = require('progress');
const { log } = require('@sturmwalzer/logger');
const { writeFileSync, existsSync } = require('fs');

const args = require('minimist')(process.argv.slice(2));

const { data, lower } = args;
const interval = args.interval || 30 * 1000;

async function main() {
  const initial = (
    await api.items.get({ promoted: 0 })
  ).items;

  const upper = initial[0].id;
  log('getting posts beween %o (hi) and %o (lo)', upper, lower);
}