import * as Router from 'koa-router';
import * as passport from 'koa-passport';

const router = new Router();
export default router;

router.get('/login', async (ctx, next) => {
  await ctx.render('login', { message: ctx.flash.get() });
});

router.post('/login', passport.authenticate('local-login', {
  successRedirect: '/',
  failureRedirect: '/login',
}));

router.get('/signup', async (ctx, next) => {
  await ctx.render('signup', { message: ctx.flash.get() });
});

router.post('/signup', passport.authenticate('local-signup', {
  successRedirect: '/',
  failureRedirect: '/signup',
}));

router.get('/logout', async (ctx, next) => {
  ctx.logout();
  ctx.redirect('/');
});
