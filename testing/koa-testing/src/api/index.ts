import { join } from 'path';
import * as Router from 'koa-router';
import { ifLoggedOn } from '../routes/router';
import { ApiKey, IApiKeyDocument } from '../models/ApiKey';

const router = new Router();
export default router;

interface IFunc {
  name: string;
  level: number;
  function: () => Promise<any>;
  description: string;
}

const names = [
  'getAccounts',
  'getCodes',
];

const functions: IFunc[] = names
  .map((name) => require('./functions/' + name).default);

async function isAuthenticated(ctx: Router.IRouterContext, level: number) {
  // One or both of these will be undefined.
  const key = ctx.params.key;
  const user = ctx.state.user;

  // If the request comes from a logged in user.
  if ((ctx.isAuthenticated && ctx.isAuthenticated()) &&
    (user && user.accessLevel <= level)
  ) {
    return true;
  }

  // If an API key was provided.
  if (key) {
    try {
      const found = await ApiKey.findOne({ key: ctx.params.key }).exec();
      return found !== null;
    } catch (err) {
      return false; // TODO: Communicate error to user.
    }
  }

  return false;
}

functions.forEach((e) => {
  router.get('/' + e.name, async (ctx, name) => {
    try {
      if (await isAuthenticated(ctx, e.level)) {
        ctx.status = 200;
        const result = await e.function();
        ctx.type = 'json';
        ctx.body = result;
      } else {
        ctx.status = 401;
        ctx.body = 'Not Authorized';
      }
    } catch (err) {
      ctx.throw(err);
    }
  });
});

router.get('/', ifLoggedOn, async (ctx) => {
  const user = ctx.state.user || {};

  const filtered = functions.filter((e) => e.level <= user.accessLevel || 3);
  const hidden = functions.length - filtered.length;

  try {
    const keys = await ApiKey.find({ owner: user.nickname });

    await ctx.render('api', { functions: filtered, hidden, keys });
  } catch (err) {
    await ctx.render('api', {
      hidden,
      functions: filtered,
      message: 'Failed to load keys.',
    });
  }
});

import keyActions from './keyActions';
router.use('/key',
  ifLoggedOn, keyActions.routes(), keyActions.allowedMethods(),
);
