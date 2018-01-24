import * as fs from 'fs';
import * as log from 'debug';
import * as path from 'path';
import * as Router from 'koa-router';

const debug = log('app:router');

const router = new Router();
export default router;

type INext = () => Promise<any>;

export async function ifLoggedOn(ctx: Router.IRouterContext, next: INext) {
  if (ctx.isAuthenticated()) {
    await next();
  } else {
    ctx.redirect('/login');
  }
}

export async function redirectIfLoggedOn(
  ctx: Router.IRouterContext,
  next: INext,
) {
  if (ctx.isAuthenticated()) {
    ctx.redirect('/');
  } else {
    await next();
  }
}

router.get('/', async (ctx, next) => {
  if (ctx.isAuthenticated()) {
    await ctx.render('index');
  } else {
    ctx.status = 200;
    ctx.type = 'html';
    ctx.body = fs.createReadStream(
      path.join(process.cwd(), 'src/public/index.html')
    );
  }
});

import userActions from './userActions';
router.use(userActions.routes());
router.use(userActions.allowedMethods());
