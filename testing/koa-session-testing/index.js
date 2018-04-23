const app = new (require('koa'))();
const route = require('koa-route');

app.keys = [ 'really secret' ];

app.use(require('koa-logger')());
app.use(require('koa-session')(app));
app.use(require('koa-bodyparser')());

app.use(route.get('/', async (ctx, next) => {
  if (ctx.session.authenticated) {
    ctx.body = 'Authenticated.';
  } else {
    ctx.body = `
      <form action="/" method="POST">
        <input name="username">
        <input name="password" type="password">
        <button type="submit">Submit</button>
      </form>
    `;
  }
}));

app.use(route.post('/', async (ctx, next) => {
  if (ctx.session.authenticated) {
    return ctx.redirect('/');
  }

  const { body } = ctx.request;

  if (body.password && body.password === 'password') {
    ctx.session.authenticated = true;
    return next();
  } else { ctx.throw(401); }
}));

app.listen(8080);
