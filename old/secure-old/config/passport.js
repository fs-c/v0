const Strategy = require('passport-local').Strategy
const User = require('../models/User')

const bcrypt = require('bcrypt-nodejs')
const log = require('../logger')

const configure = module.exports = passport => {
  passport.serializeUser((user, done) => {
    log.debug(`serializing user ${user.id}`)
    done(null, user.id)
  })

  passport.deserializeUser((id, done) => {
    User.findById(id, (err, user) => {
      log.debug(`deserializing user ${user.id}`)
      done(err, user)
    })
  })

  passport.use('local-signup', new Strategy((name, pass, done) => {
    process.nextTick(() => {
      User.findOne({ username: name }, (err, user) => {
        if (err) return done(err)
        if (user) return done(new Error('User already exists.'))

        let newUser = new User({
          username: name,
          password: bcrypt.hashSync(pass, bcrypt.genSaltSync(8))
        })

        newUser.save(err => done(err, newUser))
      })
    })
  }))

  passport.use('local-login', new Strategy((user, pass, done) => {
    process.nextTick(() => {
      User.findOne({ username: user }, (err, user) => {
        if (err) return done(err)
        if (!user) return done(null, false)
        
        bcrypt.compare(pass, user.password, (err, res) => {
          log.silly(res)
          done(err, res ? user : false)
        })
      })
    })
  }))
}