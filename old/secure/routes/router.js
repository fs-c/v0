const passport = require('passport')
const express = require('express')
const router = module.exports = express.Router()

const log = require(global.PATHS.logger)

const isLoggedIn = (req, res, next) => {
  if (req.isAuthenticated())
    return next()

  res.redirect(global.ROOT + 'login')
}

const hasAccess = level => {
  return function (req, res, next) {
    if (req.user && req.user.access <= level)
      return next()

    res.redirect(global.ROOT)
  }
}

// Serve secured pages. 
router.get('/', isLoggedIn, (req, res) => {
  res.render('index', { user: req.user, root: global.ROOT })
})

router.use('/api', isLoggedIn, require('./api'))

// Interface for steam trading related aspects of the API.
router.use('/trader', isLoggedIn, hasAccess(-1), require('../trader/'))
router.use('/trader', express.static('trader/app'))

// Idling service.
router.use('/idler', isLoggedIn, require('../idler/'))

// Generic passport boilerplate.
router.get('/login', (req, res) => {
  res.render('login', { message: req.flash('login'), root: global.ROOT })
})
router.post('/login', passport.authenticate('local-login', {
  successRedirect: global.ROOT,
  failureRedirect: global.ROOT + 'login',
  failureFlash: true
}))
  
router.get('/signup', (req, res) => {
  res.render('signup', { message: req.flash('signup'), root: global.ROOT })
})
router.post('/signup', passport.authenticate('local-signup', {
  successRedirect: global.ROOT,
  failureRedirect: global.ROOT + 'signup',
  failureFlash: true
}))

router.get('/logout', (req, res) => {
  req.logout()
  res.redirect(global.ROOT + 'login')
})