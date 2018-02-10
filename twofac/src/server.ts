import { config as configure } from 'dotenv';
configure();

import * as Koa from 'koa';
import { join } from 'path';

const app = new Koa();

app.proxy = true;
app.keys = [ process.env.SESSION_SECRET ];

import * as logger from 'koa-logger';
app.use(logger());

import * as compress from 'koa-compress';
app.use(compress());

import * as session from 'koa-session';
import * as bodyParser from 'koa-bodyparser';
app.use(bodyParser());
app.use(session({}, app));

import * as serve from 'koa-static';
app.use(serve(join(__dirname, '/public')));

import * as views from 'koa-views';
app.use(async (ctx, next) => {
  await views(
    join(__dirname, '/views'), {
      extension: 'ejs',
      options: {
        node: process.version,
        back: ctx.request.get('referer' || '/'),
        version: require('../package.json').version,
      },
    },
  )(ctx, next);
});

import router from './router';
app.use(router.routes());
app.use(router.allowedMethods());

const port = parseInt(process.env.PORT, 10) || 8080;
export const server = app.listen(port);
