const { URL } = require('url');
const { get } = require('./request');
const { getNodes } = require('./parse');
const { error, debug } = require('./log');

const args = process.argv.splice(2).reduce((acc, cur, i, arr) => {
  cur.includes('-') ? acc[cur] = true
    : acc[arr[i - 1]] = cur;
  return acc;
}, {  });

const target = new URL(args['--target']);

const processPage = async (html) => {
  const document = parse(html);
};

(async () => {
  processPage(await get(target));
})().catch(error);