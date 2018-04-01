const { get } = require('got');
const debug = require('debug')('api');
const { URLSearchParams } = require('url');

const base = 'http://pr0gramm.com/api/';

/**
 * Send a request to the given route with the params 
 * parsed as queries.
 * @param {string} route 
 * @param {object} params 
 * @returns {Promise}
 */
const request = async (
  route,
  params = { promoted: 1, flags: 1 }
) => {
  const url = base + route + '?'
    + new URLSearchParams(params).toString();
  
  debug('GET: %o', url);

  const res = await get(url, {
    json: true,
  });

  debug('%o/%o', res.statusCode, res.statusMessage);

  return res.body;
};

const api = exports.api = {
  items: {
    get: (opts) => request('items/get', opts),

    newest(opts) {
      return this.get(opts);
    },
    older(id, opts) {
      return this.get(Object.assign({ older: id }, opts));
    },
    newer(id, opts) {
      return this.get(Object.assign({ newer: id }, opts));
    },
    around(id, opts) {
      return this.get(Object.assign({ around: id }, opts));
    },

    info(id) {
      return request('items/info', { itemId: id });
    }
  }
};