const express = require('express')
const router = express.Router()
const parser = require('body-parser')

const fs = require('fs')

const log = require('winston')

router.use((req, res, next) => {
  res.setHeader('Access-Control-Allow-Origin', '*')
  res.setHeader('Access-Control-Allow-Methods', 'GET')
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  res.setHeader('Access-Control-Allow-Credentials', false)
  next()
})

router.use(parser.urlencoded({ extended: true }))

router.get('/', (req, res) => {
  res.send()
})

router.get('/:type', (req, res) => {
  let path = `./comments/${req.params.type === 'slim' ? 'slim' : 'comments'}.json`
  let data = JSON.parse(fs.readFileSync(path, 'utf8'))

  res.status(200).json(data)

  log.verbose(`GET /:${req.params.type} request served.`)
})

module.exports = router
