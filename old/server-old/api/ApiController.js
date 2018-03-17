const express = require('express')
const router = express.Router()
const parser = require('body-parser')

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
  log.verbose(`GET / served (${req.ip}).`)
  res.sendFile(__dirname + '/index.html')
})

router.use('/user', require('./user/'))
router.use('/group', require('./group/'))
// router.use('/countries', require('./countries/'))

module.exports = router
