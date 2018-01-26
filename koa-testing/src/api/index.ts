import { join } from 'path';
import * as Router from 'koa-router';
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

async function isAuthenticated(data: any, level: number) {
  // If data is an user object.
  if (data.accessLevel && data.accessLevel <= level) {
    return true;
  }

  // If data is a URL query object.
  if (data.key) {
    ApiKey.find({ key: data.key }, (err, apikey) => {
      return !err && apikey;
    });
  }

  return false;
}

functions.forEach((e) => {
  router.get('/' + e.name, async (ctx, name) => {
    try {
      if (await isAuthenticated(ctx.state.user || ctx.query || {}, e.level)) {
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

router.get('/', async (ctx) => {
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
router.use('/key', keyActions.routes());
router.use('/key', keyActions.allowedMethods());
