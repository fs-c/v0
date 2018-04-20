const _ = require('koa-route');
const app = new require('koa')();
const token = require('jsonwebtoken');
const jwt = require('koa-jwt')({ secret: String(Date.now()) });

async function authenticate(data) {
  ctx.body = {
    token: token.sign({ name: data.name }, )
  };
}

app.use(_.get('/', async (ctx) => {
  ctx.body = 'API root.';
}));

app.use(_.get('/protected', jwt, async (ctx) => {
  ctx.body = 'Protected enpoint.';
}));

app.use(_.post(ctx('/authenticate', async (ctx) => {
  authenticate(ctx.body);
})));
