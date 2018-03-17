const fs = require('fs-extra');
const debug = require('debug')('files');

const files = module.exports = (options = {}) => {
  const root = options.dir || '.';

  return async (ctx, next) => {
    const dir = await fs.readdir(root);

    debug(dir);

    ctx.body = dir;
    ctx.status = 200;
  }
}