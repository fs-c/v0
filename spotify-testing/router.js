const router = module.exports = new (require('koa-router'))();

router.get('/', async (ctx, next) => {
  if (ctx.isAuthenticated()) {
    console.log(ctx.state.user);
    await ctx.render('index', { user: ctx.state.user });
  } else {
    ctx.redirect('/login');
  }
});
