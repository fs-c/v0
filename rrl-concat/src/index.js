require('dotenv').config();

const inDev = process.env.NODE_ENV === 'dev';

const { join } = require('path');
const app = new (require('koa'))();
const route = require('koa-route');
const debug = require('debug')('rrl-concat');
const { RoyalRoadAPI } = require('@l1lly/royalroadl-api');

app.keys = [ process.env.KEY || String(Date.now()) ];

app.use(require('koa-session')(app));
app.use(require('koa-bodyparser')());
app.use(require('koa-static')(join(__dirname, 'public')));

app.use(require('koa-flash-simple')());

require('koa-ejs')(app, {
  cache: !inDev,
  viewExt: 'ejs',
  root: join(__dirname, 'views'),
});

if (inDev || process.env.DEBUG === 'rrl-concat') {
  app.use(require('koa-logger')());  
}

app.use(route.get('/', async (ctx, next) => {
  if (!ctx.session.authenticated) {
    return ctx.render('authenticate', {
      message: ctx.flash.get(),
    });
  } else {
    return ctx.render('select');
  }
}));

app.use(route.post('/', async (ctx, next) => {
  const { username, password } = ctx.request.body;

  if (!username || !password) {
    ctx.flash.set('Missing data.');
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

    if (premium) {
      ctx.session.authenticated = true;
    } else { ctx.session.authenticated = false; }
  } catch (err) {
    debug('error for %o', username);

    ctx.flash.set(err.data.message);
  }

  ctx.redirect('/');  
}));

app.listen(process.env.PORT || 8080);
