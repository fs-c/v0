const Koa = require('koa');
const path = require('path');
const debug = require('debug')('app');
const passport = require('koa-passport');

const app = new Koa();

app.keys = [ 'secret' ];

app.on('error', (err) => debug(err));
app.use(require('koa-logger')());

app.use(require('koa-session')({}, app));
app.use(require('koa-bodyparser')());

require('./auth');
app.use(passport.initialize());
app.use(passport.session());

const render = require('koa-views');
app.use(render(path.join(__dirname, '/views'), { extension: 'ejs' }));

const router = require('./router');
app.use(router.routes()).use(router.allowedMethods());

app.listen(8080);
debug('listening on 8080');