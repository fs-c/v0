const { inspect } = require('util');

const active = {
  level: process.env.LOG_LEVEL || 'info',
  modules: process.env.LOG_MODULES || '',
}

const write = (level, string) => {
  process.stdout.write(`${level} - ${string}\n`);
};

const inspectArgs = {
  depth: null,
  colors: true,
  compact: true,
};

const log = (level, body, ...args) => {
  if (typeof body !== 'string') {
    return write(level, inspect(body));
  }

  let index = -1;
  const string = body.replace(/%o/g, () => {
    index++;

    return inspect(args[index], inspectArgs);
  });

  write(level, string);
};

const levels = [ 'error', 'warn', 'info', 'debug' ];

module.exports = (sub = false) => {
  const logger = {};

  for (const level of levels) {
    const enabled = levels.indexOf(active.level) >= levels.indexOf(level)
      && sub ? active.modules.split().includes(sub) : true;

    logger[level] = enabled
      ? (...args) => log(`${level}${sub ? `:${sub}` : ''}`, ...args) : () => {};
  }

  return logger;
}