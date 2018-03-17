const router = require('express').Router()
const mongoose = require('mongoose')

router.use(require('cors')())

module.exports = router

const User = require('../models/User')

router.use((req, res, next) => {
  if (!req.user && global.DEV)
    return next()

  User.findOne({ id: req.user.id }, (err, user) => {
    if (err)
      return next(new Error('Something went wrong while getting the user.'))

    req.dbUser = user
    next()
  })
})

router.get('/', (req, res) => res.send('hello world'))

router.get('/persons', (req, res) => {
  res.json(req.dbUser.persons)
})

router.post('/persons', (req, res, next) => {
  req.dbUser.persons.push(new Person({
    name: req.body.name,
    phone: req.body.phone,
    email: req.body.email,
    birth: req.body.birth,
    notes: req.body.notes,
  })).save(err => {
    if (err)
      return next(new Error('Something went wrong while saving changes.'))

    res.json({ success: true })
  })
})

router.get('/remove/:id', (req, res) => {
  const user = req.dbUser

  user.persons.id(req.params.id).remove()

  user.save(err => {
    if (err)
      return next(new Error('Something went wrong while removing person.'))

    res.json({ success: true })
  })
})