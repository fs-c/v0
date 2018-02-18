require('dotenv').config();

const STATE_KEY = global.STATE_KEY = 'spotify_auth_key';
const DEV = global.DEV = process.env.NODE_ENV === 'dev';

const app = new (require('koa'))();

const debug = require('debug')('app:server');


// app.use(require('koa-views')('views'));
app.use(require('koa-static')('public'));

const router = require('./router');
app.use(router.routes());
app.use(router.allowedMethods());

const port = process.env.PORT || '8080';
const server = app.listen(port);