const http = require('http');

const get = (uri) => new Promise((resolve, reject) => {
  http.get(uri, (res) => {
    const { statusCode, statusMessage } = res;
    const contentType = res.headers['content-type'];

    const error = statusCode !== 200 ? (
      new Error(
        `Request failed with status code ${statusCode} (${statusMessage}).`
      )
    ) : !/^text\/html/.test(contentType) ? (
      new Error(
        `Invalid content type, expected text/html but received ${contentType}.`
      )
    ) : undefined;

    if (error) {
      res.resume(); // Free up memory.
      reject(error);
    }

    res.setEncoding('utf8');

    resolve(res);
  }).on('error', reject);
});

exports.default = exports.get = get;
