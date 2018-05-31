const AZStrategy = require('passport-auth0');
const passport = module.exports = require('koa-passport');

const log = require('./logger')('auth');

passport.serializeUser((user, done) => {
    log.trace(user, 'serializing user')
    
    done(null, user);
});

passport.deserializeUser((user, done) => {
    log.trace(user, 'deserializing user')
    
    done(null, user);
});

const { AZ_DOMAIN, AZ_CALLBACK, AZ_CLIENT_ID, AZ_CLIENT_SECRET } = process.env;
const azStrategy = new AZStrategy({
    domain: AZ_DOMAIN,
    callbackURL: AZ_CALLBACK,
    clientID: AZ_CLIENT_ID,
    clientSecret: AZ_CLIENT_SECRET,
}, (access, refresh, params, profile, done) => {
    log.trace({ access, refresh, params, profile }, 'strategy callback');

    return done(null, profile);
})

passport.use(azStrategy);
