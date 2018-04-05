const { get } = require('got');
const debug = require('debug')('api');
const { URLSearchParams } = require('url');

const base = 'http://pr0gramm.com/api/';

/**
 * Send a request to the given route with the options 
 * parsed as queries.
 * @param {string} route 
 * @param {object} options 
 * @returns {Promise}
 */
const request = async (route, options) => {
  const params = Object.assign({
    flags: 1,
    promoted: 1,
  }, options);

  const url = base + route + '?'
    + new URLSearchParams(params).toString();
  
  const res = await get(url, {
    json: true,
  });

  debug('GET: %o - %o/%o'
    , url, res.statusCode, res.statusMessage);

  return res.body;
};

const api = exports.api = {
  items: {
    get: (opts) => request('items/get', opts),
  },
};