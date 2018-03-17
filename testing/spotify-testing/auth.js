const Spotify = require('passport-spotify').Strategy;
const passport = exports.passport = require('passport');

const clientID = process.env.APP_ID;
const clientSecret = process.env.APP_SECRET;

if (!(clientID && clientSecret)) {
  throw new Error('Missing secrets.');
}

passport.serializeUser((user, done) => {
  done(null, user);
});

passport.deserializeUser((obj, done) => {
  done(null, obj);
});

passport.use(new Spotify({
  clientID,
  clientSecret,
  callbackURL: global.DEV 
    ? 'http://localhost:' + global.PORT + '/callback'
    : 'https://fsoc.space/spotify/callback',
}, (accessToken, refreshToken, expires, profile, done) => {
  console.log(accessToken, refreshToken, expires, profile);
  return done(null, profile);
}));

const router = exports.router = new (require('koa-router'))();

router.get('/login', async (ctx, next) => {
  ctx.redirect('/auth');
});

router.get(
  '/auth',
  passport.authenticate('spotify', {
    scope: [ 'user-top-read', 'user-read-recently-played' ],
    showDialog: true,
  }),
);

router.get(
  '/callback',
  passport.authenticate('spotify', { failureRedirect: '/login' }),
  async (ctx) => ctx.redirect('/'),
);

router.get('/logout', (ctx) => {
  ctx.logout();
  ctx.redirect('/');
});
