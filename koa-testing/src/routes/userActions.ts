import * as Router from 'koa-router';
import * as passport from 'koa-passport';

import { ifLoggedOn, redirectIfLoggedOn } from './router';

const router = new Router();
export default router;

router.get('/login', redirectIfLoggedOn, async (ctx, next) => {
  await ctx.render('login', { message: ctx.flash.get() });
});

router.post('/login', passport.authenticate('local-login', {
  successRedirect: '/',
  failureRedirect: '/login',
}));

router.get('/signup', redirectIfLoggedOn, async (ctx, next) => {
  await ctx.render('signup', { message: ctx.flash.get() });
});

router.post('/signup', passport.authenticate('local-signup', {
  successRedirect: '/',
  failureRedirect: '/signup',
}));

router.get('/logout', ifLoggedOn, async (ctx, next) => {
  ctx.logout();
  ctx.redirect('/');
});
