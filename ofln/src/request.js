const http = require('http');
const https = require('https');
const { debug } = require('./log')('request');

const get = (url, options) => new Promise((resolve, reject) => {
  const opts = Object.assign({
    type: false,    // Reject if content type mismatch.
    stream: false,  // Resolve with a readable stream.
  }, options);

  const requester = url.protocol === 'https:' ? https : http;

  debug('getting %o', url.href);

  requester.get(url, (res) => {
    debug('got %o (%o)', url.href, res.statusCode);

    const { statusCode, statusMessage } = res;
    const contentType = res.headers['content-type'];

    const error = statusCode !== 200 ? (
      new Error(
        `Request failed with status code ${statusCode} (${statusMessage}).`
      )
    ) : opts.type && contentType.includes(opts.type) ? (
      new Error(
        `Invalid content type, expected ${opts.type}`
        + ` but received ${contentType}.`
      )
    ) : undefined;

    if (error) {
      res.resume(); // Free up memory.
      reject(error);
    }

    if (opts.stream) {
      return resolve(res);
    }

    res.setEncoding('utf8');

    let data = '';
    res.on('data', (chunk) => { data += chunk; });

    res.on('end', () => { resolve(data); });
  }).on('error', reject);
});

exports.default = exports.get = get;
