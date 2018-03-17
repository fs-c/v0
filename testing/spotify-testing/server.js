require('dotenv').config();
global.DEV = process.env.NODE_ENV === 'dev';

const Koa = require('koa');

const app = new Koa();

app.proxy = true;
app.keys = [ process.env.SESSION_SECRET ];

app.use(require('koa-logger')());

app.use(require('koa-bodyparser')());
app.use(require('koa-session')({}, app));

const auth = require('./auth');

app.use(auth.passport.initialize());
app.use(auth.passport.session());

app.use(auth.router.routes());
app.use(auth.router.allowedMethods());

const router = require('./router');

app.use(router.routes());
app.use(router.allowedMethods());

const port = global.PORT = process.env.PORT || '8080';
const server = app.listen(port);