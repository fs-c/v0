import * as log from 'debug';
import { Context } from 'koa';
import * as bcrypt from 'bcrypt-nodejs';
import * as passport from 'koa-passport';
import { User, IUserDocument } from '../models/User';
import { Strategy as LocalStrategy } from 'passport-local';

export { passport };

const debug = log('app:auth');

passport.serializeUser((user: IUserDocument, done) => {
  done(null, user.id);
});

passport.deserializeUser((id, done) => {
  User.findById(id, (dbErr, user: IUserDocument) => {
    done(dbErr, user);
  });
});

passport.use('local-signup', new LocalStrategy({
  passReqToCallback: true,
}, (req: any, name, pass, done) => { // TODO: Dirty.
  debug(`signing up user %o`, name);

  const ctx: Context = req.ctx;

  process.nextTick(() => {
    User.findOne({ nickname: name }, (dbErr, user) => {
      if (dbErr) { return done(dbErr); }
      if (user) {
        ctx.flash.set('A user with this name already exists.');
        return done(null, false);
      }
      if (ctx.request.body.second !== pass) {
        ctx.flash.set('The passwords do not match.');
        return done(null, false);
      }

      const newUser = new User({
        nickname: name,
        password: bcrypt.hashSync(pass, bcrypt.genSaltSync(8)),
      });

      newUser.save((saveErr) => done(saveErr, newUser));
    });
  });
}));

passport.use('local-login', new LocalStrategy({
  passReqToCallback: true,
}, (req: any, name, pass, done) => { // TODO: Dirty.
  debug(`logging in user %o`, name);

  const ctx: Context = req.ctx;

  process.nextTick(() => {
    User.findOne({ nickname: name }, (dbErr, user) => {
      if (dbErr) { return done(dbErr); }
      if (!user) {
        ctx.flash.set('The given user was not found.');
        return done(null, false);
      }

      bcrypt.compare(pass, user.password, (err, valid: boolean) => {
        if (valid) {
          done(null, user);
        } else {
          ctx.flash.set('The given password is incorrect.');
          done(null, false);
        }
      });
    });
  });
}));