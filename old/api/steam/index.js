const express = require('express')
const router = express.Router()

module.exports = router

const Limiter = require('express-rate-limit')
const parser = require('body-parser')

router.use(parser.urlencoded({ extended: true }))

router.use('/comments/:id', require('./comments/'))

let limiters = {
  steamAPI: new Limiter({
    windowMs: 60 * 60 * 1000,
    delayAfter: 0,
    // SteamAPI daily limit/IP is 100k, with some exceptions.
    max: process.env.NODE_ENV === 'dev' ? 0 : 1000,
    headers: true
  }),
  market: new Limiter({
    windowMs: 60 * 60 * 1000,
    delayAfter: 0,
    max: process.env.NODE_ENV === 'dev' ? 0 : 100,
    headers: true
  }),
  inventory: new Limiter({
    windowMs: 60 * 60 * 1000,
    delayAfter: 0,
    max: process.env.NODE_ENV === 'dev' ? 0 : 10,
    headers: true
  })
}

const Community = require('steamcommunity')
const SteamID = Community.SteamID

let web = new Community()

// SteamID/vanityURL conversion.
router.get('/id/:id', limiters.steamAPI, (req, res) => {
  try {
    let id = new SteamID(req.params.id)
    send(id)
  } catch (e) {
    require('request')(`http://api.steampowered.com/ISteamUser/ResolveVanityURL/v0001/?key=${require('../../../steamdata').main.apikey}&vanityURL=${req.params.id}`, (err, r, body) => {
      if (err) res.json({ error: 'Request failed.' })
      try {
        send(new SteamID(JSON.parse(body).response.steamid))
      } catch (e) { res.json({ error: 'Invalid.' }) }
    })
  }

  function send (id) {
    res.json({ data: {
      object: id,
      steam2: id.getSteam2RenderedID(id),
      steam3: id.getSteam3RenderedID(id),
      steam64: id.getSteamID64(id)
    }})
  }
})

// Steamcommunity.
router.get('/user/:id', limiters.steamAPI, (req, res) => {
  let id
  try { id = new SteamID(req.params.id) } catch (e) { id = req.params.id }

  web.getSteamUser(id, (err, user) => {
    if (err) return res.json({ error: `${err.msg || err.message || err}` })
    user.avatarURL = user.getAvatarURL()
    delete user._community
    res.json({ data: user })
  })
})

router.get('/user/:id/inventory', limiters.inventory, (req, res) => {
  try {
    let q = req.query
    web.getUserInventoryContents(new SteamID(req.params.id), q.appid, q.context, q.tradable = true, (err, inv) => {
      if (err) return res.json({ error: `${err.msg || err.message || err}` })
      res.json({ data: inv })
    })
  } catch (e) { res.json({ error: 'Invalid.' }) }
})

router.get('/user/:id/contexts', limiters.steamAPI, (req, res) => {
  try {
    web.getUserInventoryContexts(new SteamID(req.params.id), (err, apps) => {
      if (err) return res.json({ error: `${err.msg || err.message || err}` })
      res.json({ data: apps })
    })
  } catch (e) { res.json({ error: 'Invalid.' }) }
})

router.get('/group/:id', limiters.steamAPI, (req, res) => {
  let id
  try { id = new SteamID(req.params.id) } catch (e) { id = req.params.id }

  web.getSteamGroup(id, (err, group) => {
    if (err) return res.json({ error: `${err.msg || err.message || err}` })
    group.avatarURL = group.getAvatarURL()
    delete group._community
    res.json({ data: group })
  })
})

router.get('/market/item/:appid/:hash', limiters.market, (req, res) => {
  web.getMarketItem(req.params.appid, req.params.hash, (err, item) => {
    if (err) return res.json({ error: `${err.msg || err.message || err}` })
    for (let n of Object.keys(item)) if (n[0] === '_') delete item[n]
    res.json({ data: item })
  })
})
