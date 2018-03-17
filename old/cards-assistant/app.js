const log = require('./logger')

const config = require('./config')
const badges = require('./badges')
const inventory = require('./inventory')
const card = require('./card')

badges.get(config.partner, (err, badges) => {
  if (err) log.error(`${err.message} for ${config.partner})`)

  let partnerIDs = []
  for (let badge of badges) {
    if (badge.appid) partnerIDs.push(badge.appid)
  }

  log.info(`found ${partnerIDs.length} games that partner has badges for.`)

  let ownerCards = {}
  inventory.load(config.owner, (err, items) => {
    if (err) log.error(`${err.message} for ${config.owner}`)

    for (let item of items) {
      let id = card.parse(item)
      if (id) {
        if (ownerCards[id]) { ownerCards[id].push(item.market_hash_name) }
        else { ownerCards[id] = [ item.market_hash_name ] }
      }
    }

    log.info(`parsed owners items and found trading cards for ${Object.keys(ownerCards).length} games.`)

    inventory.parse(ownerCards, sets => {
      console.log(sets)
    })
  })
})
