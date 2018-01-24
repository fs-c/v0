const Router = require('koa-router');
const passport = require('koa-passport');
const debug = require('debug')('app:router');

const router = module.exports = new Router();

async function ifLoggedOn(ctx, next) {
  if (ctx.isAuthenticated()) {
    await next();
  } else {
    ctx.redirect('/');
  }
}

async function redirectIfLoggedOn(ctx, next) {
  if (ctx.isAuthenticated()) {
    ctx.redirect('/secure');
  } else {
    await next();
  }
}

router.get('/', redirectIfLoggedOn, async (ctx) => {
  await ctx.render('index');
});

router.get('/secure', ifLoggedOn, async (ctx) => {
  await ctx.render('secure', { user: ctx.user });
})

router.get('/login', redirectIfLoggedOn, async (ctx) => {
  await ctx.render('login');
});

router.post('/login', passport.authenticate('local', {
  successRedirect: '/secure',
  failureRedirect: '/login'
}));

router.get('/logout', ifLoggedOn, (ctx) => {
  ctx.logout();
  ctx.status = 200;
  ctx.body = 'Logged out.'
});