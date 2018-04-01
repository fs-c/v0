const { get } = require('got');

const base = 'http://pr0gramm.com/api/';

/**
 * Send a request to the given route with the params parsed as queries.
 * @param {string} route 
 * @param {object} params 
 * @returns {Promise}
 */
const request = async (route, params = { promoted: 1, flags: 1 }) => {
  return (await get(base + route, {
    json: true,
    query: params,
  })).body;
};

const api = exports.api = {
  items: {
    newest: (opts) => request('items/get', opts),
    older(id, opts) {
      return request('items/get', Object.merge({ older: id }, opts));
    },
    newer(id, opts) {
      return request('items/get', Object.merge({ newer: id }, opts));
    },
    around(id, opts) {
      return request('items/get', Object.merge({ around: id }, opts));
    },
  }
};