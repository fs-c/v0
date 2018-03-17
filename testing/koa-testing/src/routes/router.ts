import * as fs from 'fs';
import * as log from 'debug';
import * as path from 'path';
import * as Router from 'koa-router';

const debug = log('app:router');

const router = new Router();
export default router;

type INext = () => Promise<any>;

export function ifLoggedOn(ctx: Router.IRouterContext, next: INext) {
  if (ctx.isAuthenticated()) {
    return next();
  } else {
    ctx.redirect('/login');
  }
}

export function redirectIfLoggedOn(
  ctx: Router.IRouterContext,
  next: INext,
) {
  if (ctx.isAuthenticated()) {
    ctx.redirect('/');
  } else {
    return next();
  }
}

router.get('/', async (ctx, next) => {
  if (ctx.isAuthenticated()) {
    // Render index.ejs if authenticated.
    await ctx.render('index');
  } else {
    // Send the index.html if not authenticated.
    ctx.status = 200;
    ctx.type = 'html';
    ctx.body = fs.createReadStream(
      path.join(process.cwd(), 'src/public/index.html'),
    );
  }
});

import userActions from './userActions';
router.use(userActions.routes());
router.use(userActions.allowedMethods());

import api from '../api/';
router.use('/api', api.routes(), api.allowedMethods());
