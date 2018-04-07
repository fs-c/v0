const { api } = require('./pr0');
const { join } = require('path');
const { log } = require('@sturmwalzer/logger');
const { writeFileSync, existsSync } = require('fs');

const args = require('minimist')(process.argv.slice(2));

const { data, lower } = args;
const interval = args.interval || 500;

let interval;
const stack = [];

async function main() {
  const items = await api.items.newest({ promoted: 0 });

  const upper = items[0].id;
  log('getting posts beween %o (hi) and %o (lo)', upper, lower);

  for (const { id } of items) {
    stack.push(id);
  }
  
  interval = setInterval(tick, interval);
}

/**
 * Process lowest (oldest) item on stack, if the stack's size 
 * is one, get fresh items.
 */
let ticks = 0;
async function tick() {
  ticks++;

  const active = stack[0];  
  stack.splice(0, 1);

  if (active == lower) {
    log('done');
    clearInterval(interval);
    return;
  }

  if (stack.length === 0) {
    const older = await api.items.older(
      active.id, { promoted: 0 }
    );

    for (const { id } of older) {
      stack.push(id);
    }
  }

  const path = join(data, `${active}.json`);

  if (existsSync(path)) {
    return log(
      'tick %o: '.padEnd(20) + '%o was already processed',
      ticks, active
    );
  }

  const item = await api.items.info(active);

  writeFileSync(path, JSON.stringify(item));

  log('tick %o: '.padEnd(20) + 'processed %o', active);
}

main(); 