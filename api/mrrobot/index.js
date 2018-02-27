const express = require('express')
const router = express.Router()

module.exports = router

const parser = require('body-parser')

router.use(parser.urlencoded({ extended: true }))

let quotes = require('./quotes')
let updated = Date.now()

router.get('/', (req, res) => {
  if (updated + 60 * 60 * 1000 < Date.now()) {
    require('fs').readFile('./quotes.json', (err, data) => {
      if (!err) quotes = JSON.parse(data)
      res.status(200).send(quotes[Math.floor(Math.random() * quotes.length)])
    })
  } else res.status(200).send(quotes[Math.floor(Math.random() * quotes.length)])
})
