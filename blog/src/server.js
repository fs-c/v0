const Koa = require('koa');

const { log } = require('./logger');

const app = exports.app = new Koa();

log.debug('bla');