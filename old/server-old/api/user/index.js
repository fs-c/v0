const express = require('express')
const router = express.Router()
const parser = require('body-parser')

const log = require('winston')

const RateLimit = require('express-rate-limit')
router.use(new RateLimit({
  windowMs: 15 * 60 * 1000,
  max: 250,
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

const User = require('./User')
router.get('/:id', (req, res) => {
  let id = req.params.id

  function send (user) {
    res.status(200).send({
      steamID: user.steamID,
      name: user.name,
      onlineState: user.onlineState,
      stateMessage: user.stateMessage,
      privacyState: user.privacyState,
      vacBanned:  user.vacBanned,
      tradeBanState: user.tradeBanState,
      memberSince: user.memberSince,
      customURL: user.customURL,
      location: user.location,
      realName: user.realName,
      summary: user.summary,
      groups: user.groups,
      primaryGroup: user.primaryGroup,
      avatar: user.getAvatarURL()
    })
  }

  User(id)
  .then(user => {
    send(user)
    log.verbose(`GET api/user/:${id} request served (${req.ip}).`)
  })
  .catch(err => {
    res.status(400).send(`error: ${err}`)
    log.verbose(`GET api/user/:${id} request failed (${req.ip}).`)
  })
})

const Inventory = require('./Inventory')
router.get('/:id/inventory', (req, res) => {
  let id = req.params.id
  let appID = req.query.appID
  let contextID = req.query.contextID

  Inventory(id, appID, contextID)
  .then(inv => {
    res.status(200).send(inv)
    log.verbose(`GET api/user/:${id}/inventory request served (${req.ip}).`)
  })
  .catch(err => {
    res.status(400).send(`error: ${err}`)
    log.verbose(`GET api/user/:${id}/inventory request failed (${req.ip}).`)
  })
})

module.exports = router
