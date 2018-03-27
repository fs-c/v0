require('dotenv').config();

const Koa = require('koa');
const { join } = require('path');
const { log } = require('./logger');

const app = exports.app = new Koa();

app.proxy = true;
app.keys = [ process.env.SESSION ];

const compress = require('koa-compress'); 
app.use(compress());

const serve = require('koa-static');
app.use(serve(join(__dirname, 'public')));

const render = require('koa-views');
app.use(render(join(__dirname, 'views'), {
  extension: 'ejs',
}));

app.use(async (ctx, next) => {
  try { await next() } catch(err) {
    ctx.render('status', { err });

    log.warn(err.message);
  }
})