const { URL } = require('url');
const { parse } = require('parse5');
const { get } = require('./request');
const { error, debug } = require('./log');

const args = process.argv.splice(2).reduce((acc, cur, i, arr) => {
  cur.includes('-') ? acc[cur] = true
    : acc[arr[i - 1]] = cur;
  return acc;
}, {  });

const target = new URL(args['--target']);

const getNode = (parent, name) => getNodes(parent, name, 1)[0];
const getNodes = (parent, name, max) => {
  let nodes = [];

  const traverseNodes = (node) => {
    const children = node.childNodes;

    if (children) {
      debug('traversing %o (%o children)', parent.nodeName, children.length)

      for (const child of children) {
        if (!name || (name && child.nodeName === name)) {
          debug('found node %o', child.nodeName);

          nodes.push(child);
        }

        if (!max || (max && max > nodes.length)) {
          traverseNodes(child);
        }
      }
    }
  }

  traverseNodes(parent);

  return nodes;
};

const processPage = async (html) => {
  const document = parse(html);
};

(async () => {
  processPage(await get(target));
})().catch(error);