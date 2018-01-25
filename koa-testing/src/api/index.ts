import { join } from 'path';
import * as Router from 'koa-router';
import { ApiKey, IApiKeyDocument } from '../models/ApiKey';

const router = new Router();
export default router;

type func = () => Promise<any>;

interface IFunc {
  name: string;
  level: number;
  description: string;
  function: func;
}

const names = [
  'getAccounts',
];

const functions: any[] = names.map((e) => require('./' + e).default);

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
        const result = await e.function();
        ctx.status = 200;
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
