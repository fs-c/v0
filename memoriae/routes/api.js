const express = require('express')
const mongoose = require('mongoose')

const Person = require('../schemas/person')

const router = express.Router()

router.get('/persons', (req, res) => {
  Person.find({  }).then(res.json)
})

router.post('/persons', (req, res) => {
  new Person({
    name: req.body.name,
    phone: req.body.phone,
    email: req.body.email,
    birth: req.body.birth,
    notes: req.body.notes,
  }).save((err, person) => console.log(err, person))
})

module.exports = router