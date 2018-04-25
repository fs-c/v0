require('dotenv').config();

const inDev = process.env.NODE_ENV === 'dev';

const { join } = require('path');
const app = new (require('koa'))();
const route = require('koa-route');
const debug = require('debug')('rrl-concat');
const { RoyalRoadAPI } = require('@l1lly/royalroadl-api');

// Global RRL API class, not to be used for logins.
const rrl = new RoyalRoadAPI();

app.keys = [ process.env.KEY || String(Date.now()) ];

app.use(require('koa-bodyparser')());
app.use(require('koa-static')(join(__dirname, 'public')));
app.use(require('koa-session')(app, {
  rolling: true,
  maxAge: 1000 * 60 * 60 * 24 * 7,
}));

require('koa-ejs')(app, {
  cache: !inDev,
  viewExt: 'ejs',
  root: join(__dirname, 'views'),
});

if (inDev || process.env.DEBUG === 'rrl-concat') {
  app.use(require('koa-logger')());  
}

app.use(async (ctx, next) => {
  const { flash } = ctx.session;

  Object.defineProperty(ctx, 'flash', {
    set: (data) => ctx.session.flash = data,
    get: () => {
      ctx.session.flash = null;
      return flash || null;
    },
  });

  await next();
});

app.use(async (ctx, next) => {
  try {
    await next();
  } catch (err) {
    debug(err);

    return ctx.render('status', { err })
  }
});

app.use(route.get('/', async (ctx, next) => {
  if (!ctx.session.authenticated) {
    return ctx.render('authenticate', {
      message: ctx.flash,
    });
  }

  const { fiction, size = 5, page = 0 } = ctx.query;
  if (fiction) {
    const meta = (await rrl.fiction.getFiction(fiction)).data
      .chapters.slice(page * size, (page * size) + 10);
    debug('got metadata for fiction %o', fiction);

    const content = (await Promise.all(
      meta.map((chap) => rrl.chapter.getChapter(chap.id)),
    )).map((res) => res.data);
    debug('got content for %o chapters', content.length);

    const chapters = content.map((e, i) => Object.assign(e, meta[i]));

    return ctx.render('fiction', { chapters });
  }

  return ctx.render('select');
}));

app.use(route.post('/authenticate', async (ctx, next) => {
  const { username, password } = ctx.request.body;

  if (!username || !password) {
    ctx.flash = 'Missing data.';
    return ctx.redirect('/');
  }

  const api = new RoyalRoadAPI();

  try {
    await api.user.login(username, password);
    debug('%o logged in to RRL', username);

    await api.chapter.postComment(208170, String(Date.now()));
    debug('posted comment');
  
    const comments = (await api.chapter.getComments(208170, 'last')).data;
    debug('got comments');

    const premium = comments[comments.length - 1].author.premium;
    debug('%o: %o', username, premium);

    ctx.session.authenticated = premium;

    if (!premium) {
      ctx.flash = 'You don\'t have premium.';
    }
  } catch (err) {
    debug('error for %o (%o)', username, err.message);

    ctx.flash = err.data.message;
  }

  ctx.redirect('/');  
}));

app.listen(process.env.PORT || 8080);
