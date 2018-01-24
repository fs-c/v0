import * as Koa from 'koa'; // TODO: Importing just for declarations.
import * as logger from 'debug';

const debug = logger('urlparser');

type INext = () => Promise<any>;

export default function urlparser() {
  // TODO: Why does next: Promise<any> make tslint complain?
  return async (ctx: Koa.Context, next: INext) => {
    const url = ctx.url;

    if (!url.includes('?') || !url.includes('=')) {
      await next();
      return;
    }

    let queries: any = {};
    const querystring = url.slice(url.indexOf('?') + 1);

    if (url.includes('&')) {
      queries = querystring
        .split('&')
        .reduce((acc: any, cur: string, i: number, arr: string[]) => {
          const pair = cur.split('=');

          if (acc[pair[0]]) {
            acc[pair[0]] = pair[1];
          } else {
            acc[pair[0]] = true;
          }

          return acc;
        }, {});
    } else {
      const pair = querystring.split('=');

      queries[pair[0]] = pair[1];
    }

    debug(queries);

    ctx.query = queries;
    await next();
    return;
  };
}
