const inDev = process.env.NODE_ENV !== 'production';

if (inDev) {
  require('debug').enable('discord');
}

exports.Client = require('./lib/Client');
