import * as Router from 'koa-router';

const router = new Router();
export default router;

router.get('/', async (ctx, next) => {
  ctx.type = 'html';
  ctx.body = 'Sometimes I don\'t sleep. </br><a href="/login">Log in.</a>';
});

import userActions from './userActions';
router.use(userActions.routes());
router.use(userActions.allowedMethods());
