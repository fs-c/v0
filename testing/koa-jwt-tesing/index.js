const secret = Date.now();

const _ = require('koa-route');
const app = new (require('koa'))();
const token = require('jsonwebtoken');

app.use(require('koa-logger')());

app.use(async (ctx, next) => {
  try {
    await next()
  } catch (err) {
    if (err.status === 401) {
      ctx.body = `
        <form method="POST" action="/">
          <input name="name">
          <input name="password" type="password">
          <button type="submit">Submit</button>
        </form>
      `;
    }
  }
});

app.use(_.post('/', async (ctx, next) => {
  console.log('reached the / post function');

  const { body } = ctx.request.body;

  if (data.password !== 'pass') {
    return ctx.throw(401);
  }

  ctx.body = {
    token: token.sign({
      data: body,
      exp: Math.floor(Date.now() / 1000) + (60 * 60),
    }, secret),    
  };

  ctx.redirect('/');

  await next();
}));

app.use(require('koa-jwt')({ secret }));

app.use(_.get('/', async (ctx) => {
  ctx.body = 'Protected.';
}));

app.listen(8080);
