const router = require('express').Router()
const passport = require('passport')

module.exports = router

router.get('/', (req, res) => res.send('hello world'))

router.get('/facebook', passport.authenticate('facebook', { scope: 'email' }))

router.get('/facebook/callback', passport.authenticate('facebook', {
  successRedirect: '/',
  failureRedirect: '/error'
}))