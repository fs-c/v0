const router = require('express').Router()
const mongoose = require('mongoose')

const Person = require('../schemas/person')

router.get('/persons', (req, res) => {
  Person.find({  }, (err, result) => {
    if (err) return res.status(400).send()
    res.json(result)
  })
})

router.post('/persons', (req, res) => {
  new Person({
    name: req.body.name,
    phone: req.body.phone,
    email: req.body.email,
    birth: req.body.birth,
    notes: req.body.notes,
  }).save(err => err && console.error(err))
})

router.get('/remove', (req, res) => {
  Person.findOneAndRemove({ _id: req.body.id }, err => {
    res.json({ success: Boolean(err) })
  })
})

module.exports = router