const FacebookStrategy = require('passport-facebook').Strategy
const User = require('../models/User')

const config = require('./auth')

const configure = passport => {
  passport.serializeUser((user, done) => {
    done(null, user.id)
  })

  passport.deserializeUser((id, done) => {
    User.findById(id, (err, user) => {
      done(err, user)
    })
  })

  passport.use(new FacebookStrategy({
    clientID: config.facebook.clientID,
    clientSecret: config.facebook.clientSecret,
    callbackURL: global.DEV 
      ? 'http://localhost:8080/auth/facebook/callback'
      : 'https://fsoc.space/memoriae/auth/facebook/callback',
      profileFields: ['id', 'email', 'first_name', 'last_name']
  }, (token, refresh, profile, done) => {
    process.nextTick(() => {
      User.findOne({ id: profile.id }, (err, user) => {
        if (err) return done(err)
        if (user) return done(null, user)

        
        let newUser = new User({
          token,
          id: profile.id,
          name: profile.name.givenName + ' ' + profile.name.familyName,
          email: (profile.emails[0].value || '').toLowerCase(),
          persons: []
        })

        newUser.save(err => {
          if (err) return done(err)
          return done(null, user)
        })
      })
    })
  }))
}

module.exports = configure