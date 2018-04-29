require('dotenv').config();

const inDev = process.env.NODE_ENV === 'dev';

const Koa = require('koa');
const { join } = require('path');
const route = require('koa-route');
const debug = require('debug')('rrl-concat');
const { RoyalRoadAPI, RoyalError } = require('@l1lly/royalroadl-api');

const app = exports.app = new Koa();

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

app.use(async (ctx, next) => {
  if (inDev) {
    ctx.session.authenticated = true;

    app.use(require('koa-logger')());
  }

  await next();
});

/**
 * Really simple flash middleware for passing status messages down to views.
 */
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

/**
 * Render all errors in the status view, it'll know what to do.
 */
app.use(async (ctx, next) => {
  try {
    await next();
  } catch (err) {
    debug(err);

    const royal = err instanceof RoyalError ? err : undefined;
    return ctx.render('status', { err, royal })
  }
});

/**
 * If not authenticated, render authentication view - else render selection
 * view.
 * 
 * If a fiction query is passed, get fiction and chapter data, and render the 
 * fiction view with the relevant chapters.
 */
app.use(route.get('/', async (ctx, next) => {
  if (!ctx.session.authenticated) {
    return ctx.render('authenticate', {
      message: ctx.flash,
    });
  }

  const { fiction, chapter, size = 5, page = 0 } = ctx.query;
  if (fiction) {
    const meta = (await rrl.fiction.getFiction(fiction)).data
    debug('got metadata for fiction %o', fiction);

    const index = chapter ? (
      meta.chapters.indexOf(
        meta.chapters.filter((chap) => chap.id === chapter)
      )
    ) : 0;

    // Slice into chunks of the given size, at the given chunk index.    
    const chunk = !chapter ? (
      meta.chapters.slice(page * size, (page * size) + parseInt(size, 10))
    ) : (
      meta.chapters.slice(index + (page * size), index + (page * size) + size)
    );

    // Get all chapters at once, to minimise load times.
    const content = (await Promise.all(
      chunk.map((chap) => rrl.chapter.getChapter(chap.id)),
    )).map((res) => res.data);
    debug('got content for %o chapters', content.length);

    const chapters = content.map((e, i) => Object.assign(e, chunk[i]));

    return ctx.render('fiction', { chapters, fiction, size, page });
  }

  return ctx.render('select');
}));

/**
 * Authenticate a user by checking if they are a RRL premium subscriber.
 * 
 * Do this by:
 *    - logging on to RRL with the data provided
 *    - posting a comment as the user
 *    - reading comment data, revealing if the account has a premium badge
 */
app.use(route.post('/authenticate', async (ctx, next) => {
  const { username, password } = ctx.request.body;

  if (!username || !password) {
    ctx.flash = 'Missing data.';
    return ctx.redirect('/');
  }

  // Create a new instance for every request, since we want to discard the 
  // internal requester.
  const api = new RoyalRoadAPI();

  try {
    await api.user.login(username, password);
    debug('%o logged in to RRL', username);

    const verification = String(Date.now());

    await api.chapter.postComment(208170, verification);
    debug('posted comment');
  
    const comments = (await api.chapter.getComments(208170, 'last')).data;
    debug('got comments');

    // The last comment should be the one we just posted.
    const premium = comments.filter((cmt) =>
      cmt.content.includes(verification)
    )[0].author.premium;

    debug('%o: %o', username, premium);

    ctx.session.authenticated = premium;

    if (!premium) {
      ctx.flash = 'You don\'t have premium.';
    }
  } catch (err) {
    debug('error for %o (%o)', username, err.message);

    ctx.flash = err.data.message;
  }

  // Redirect whether the user has premium or not.
  ctx.redirect('/');  
}));

if (require.main === module) {
  app.listen(process.env.PORT || 8080);
} else { /* We were require()d, don't start the server. */ }
