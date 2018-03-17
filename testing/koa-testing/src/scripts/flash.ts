import { Context } from 'koa';
import * as logger from 'debug';

const debug = logger('flash');

type INext = () => Promise<any>;

export default function flash() {
  return (ctx: Context, next: INext) => {
    const prev = ctx.session.flash;

    if (prev) {
      debug('message found %j', prev);
      ctx.session.flash = null;
    } else {
      debug('no message found');
    }

    ctx.flash = {
      get: () => prev,
      set: (data: any) => ctx.session.flash = data,
    };

    return next();
  };
}
