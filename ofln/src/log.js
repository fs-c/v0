const { inspect } = require('util');

const active = process.env.LOG_LEVEL || 'info';

const write = (level, string) => {
  process.stdout.write(`${level}: ${string}\n`);
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

exports.default = exports.log = log;

const levels = [ 'error', 'warn', 'info', 'debug' ];
for (const index in levels) {
  const level = levels[index];
  const enabled = levels.indexOf(active) >= index;

  exports[level] = (...args) => enabled ? log(level, ...args) : null;
}
