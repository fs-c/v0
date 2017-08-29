const express = require('express')
const router = express.Router()
const parser = require('body-parser')

const log = require('winston')

const C = require('steamcommunity')
let cm = new C()

let cache = []

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

router.get('/:id', (req, res) => {
  let id = req.params.id
  log.verbose(`GET users/:${id} request received.`)

  function send (user) {
    res.status(200).send({
      name: user.name,
      onlineState: user.onlineState,
      vacBanned:  user.vacBanned,
      customURL: user.customURL,
      location: user.location,
      realName: user.realName,
      summary: user.summary,
      groups: user.groups,
      primaryGroup: user.primaryGroup,
      avatar: user.getAvatarURL()
    })
  }

  function cached (id) {
    let c = false
    for (let i in cache) if (i === id) c = true
    return c
  }

  if (id.length === 17) {
    // If there is a cached profile with that ID, and the cache was created <10 minutes ago, use it.
    if (cached(id) && ((cache[id].updated + 10*60*1000) > Date.now())) {
      log.debug(`got profile ${id} from cache.`)
      send(cache[id].user)
      return
    }

    cm.getSteamUser(new (C.SteamID)(id), (err, user) => {
      if (err) {
        log.error(`error while getting steam profile for ${id}: ${err.message}`)
        res.status(400).send('Something went wrong while getting steam profile.')
        return
      }

      log.info(`got steam profile for ${id}.`)

      send(user)

      cache[id] = { user, updated: Date.now() }
      log.debug(`added user ${id} to cache.`)

      if (Object.keys(cache).length > 100) {
        for (let id of Object.keys(cache).slice(99)) {
          delete cache[id]
          log.debug(`removed user ${id} from cache.`)
        }
      }
    })
  } else res.status(404).send('Want some tea?')
})

module.exports = router
