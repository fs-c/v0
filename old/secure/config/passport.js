const Strategy = require('passport-local').Strategy
const User = require('../models/User')

const bcrypt = require('bcrypt-nodejs')
const log = require(global.PATHS.logger)

const configure = module.exports = passport => {
  passport.serializeUser((user, done) => {
    done(null, user.id)
  })

  passport.deserializeUser((id, done) => {
    User.findById(id, (err, user) => {
      done(err, user)
    })
  })

  passport.use('local-signup', new Strategy({
    passReqToCallback : true 
  }, (req, name, pass, done) => {
    process.nextTick(() => {
      User.findOne({ username: name }, (err, user) => {
        if (err) return done(err)
        if (user)
          return done(null, false, req.flash('signup', 'User already exists.'))

        let newUser = new User({
          username: name,
          password: bcrypt.hashSync(pass, bcrypt.genSaltSync(8))
        })

        newUser.save(err => done(err, newUser))
      })
    })
  }))

  passport.use('local-login', new Strategy({
    passReqToCallback: true
  }, (req, user, pass, done) => {
    process.nextTick(() => {
      User.findOne({ username: user }, (err, user) => {
        if (err) return done(err)
        if (!user) return done(null, false, req.flash('login', 'No user found.'))
        
        bcrypt.compare(pass, user.password, (err, res) => {
          done(err, res ? user : false,
            !res ? req.flash('login', 'Incorrect password.') : null)
        })
      })
    })
  }))
}