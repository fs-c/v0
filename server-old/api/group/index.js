const express = require('express')
const router = express.Router()
const parser = require('body-parser')

const fs = require('fs')

const log = require('winston')

const RateLimit = require('express-rate-limit')
router.use(new RateLimit({
  windowMs: 15 * 60 * 1000,
  max: 100,
  delayMs: 0,
  headers: true
}))

router.use((req, res, next) => {
  res.setHeader('Access-Control-Allow-Origin', '*')
  res.setHeader('Access-Control-Allow-Methods', 'GET')
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  res.setHeader('Access-Control-Allow-Credentials', false)
  next()
})

router.use(parser.urlencoded({ extended: true }))

router.get('/', (req, res) => {
  res.send('Did you get lost?')
})

router.get('/comments/:type', (req, res) => {
  let path = `./api/group/comments/${req.params.type === 'slim' ? 'slim' : 'comments'}.json`
  let data = JSON.parse(fs.readFileSync(path, 'utf8'))

  res.status(200).json(data)

  log.verbose(`GET api/group/comments/:${req.params.type} request served.`)
})

module.exports = router
