const { inspect } = require('util');

/**
 * Prepend zeroes to val until it reaches a length of len.
 * @param {number|string} val 
 * @param {number} [len=2] 
 * @returns {string}
 */
const pad = (val, len = 2, str = String(val)) => {
  while (str.length < len) { str = '0' + str; }
  return str;
}

/**
 * Returns a formatted string representing the current local time. 
 * @returns {string}
 */
const date = () => {
  const now = new Date();

  return `${now.getHours()}:`
    + `${pad(now.getMinutes())}:`
    + `${pad(now.getSeconds())}:`
    + `${pad(now.getMilliseconds())}`;
}

/**
 * Writes the given string with a prepended date and an appended newline 
 * to stdout.
 * @param {string} string 
 */
const write = (string) => {
  process.stdout.write(`${date()} - ${string}\n`);
};

const inspectArgs = {
  depth: null,
  colors: true,
  compact: true,
};

/**
 * Logs body to console and arguments to console.
 * If body is a string, formats it printf-like with the additional arguments.
 * @param {*} body
 * @param {...*} arguments
 */
const log = exports.log = (body, ...args) => {
  if (typeof body !== 'string') {
    return write(inspect(body, inspectArgs));
  }

  let index = 0;
  const string = body.replace(/%o/g, (match, format) => {
    return inspect(args[index++], inspectArgs);
  });

  write(string);
};