const passport = require('koa-passport');
const debug = require('debug')('app:auth');
const Strategy = require('passport-local').Strategy;

const userData = { id: 1, name: 'test', pass: '1234' };

passport.serializeUser((user, done) => {
  debug('serializing user %o', user)
  done(null, user.id);
});

passport.deserializeUser((id, done) => {
  debug('deserializing user %o', userData)
  done(null, userData);
});

passport.use(new Strategy((name, pass, done) => {
  if (userData.name === name && userData.pass === pass) {
    done(null, userData);
  } else {
    done(null, false);
  };
}));
