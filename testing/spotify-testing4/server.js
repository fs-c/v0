require('dotenv').config();

const app = new (require('koa'))();
const debug = require('debug')('app:server');

app.use(async (ctx, next) => {
  if (ctx.request.url.includes('/topify')) {
    ctx.redirect(ctx.request.url.replace('/topify', ''));
  } else await next();
});

app.use(async (ctx, next) => {
  if (!ctx.cookies.get('spotify_access_token')) {
    const spotify = new (require('spotify-web-api-node'))({
      clientId: process.env.CLIENT_ID,
      redirectUri: ctx.origin + '/topify/callback',
    });
  }
});

app.use(require('views')());

const port = process.env.PORT || '8080';
const server = app.listen(port);
debug('listening on %o', port);
