const IN_DEV = process.env.NODE_ENV === 'dev';

const Koa = require('koa');
const { get, post } = require('koa-route');
const log = require('pino')({
    base: null,
    name: 'server',
    level: IN_DEV ? 'trace' : 'info',
});

const app = new Koa();
