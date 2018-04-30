const { debug } = require('./log');
const { parse } = require('parse5');

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
exports.parse = parse;
exports.getNode = getNode;
exports.default = exports.getNodes = getNodes;