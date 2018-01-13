const log = require('../logger')

const isLoggedIn = (req, res, next) => {
  if (req.isAuthenticated())
    return next()

  res.redirect('login')
}

const hasAccess = level => {
  return function (req, res, next) {
    if (req.user && req.user.access <= level)
      return next()

    res.redirect('/')
  }
}

module.exports = (app, passport) => {
  app.get('/', isLoggedIn, require('./index/'))

  app.use('/api', isLoggedIn, hasAccess(0), require('./api/'))

  app.get('/login', (req, res) => { res.render('login') })
  app.post('/login', passport.authenticate('local-login', {
    successRedirect: '/',
    failureRedirect: '/login'
  }))
  
  app.get('/signup', (req, res) => { res.render('signup') })
  app.post('/signup', passport.authenticate('local-signup', {
    successRedirect: '/',
    failureRedirect: '/signup'
  }))

  app.get('/logout', (req, res) => {
    req.logout()
    res.redirect('login')
  })
}