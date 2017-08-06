const log = require('./logger')

const config = require('./config')
const badges = require('./badges')
const inventory = require('./inventory')
const card = require('./card')

badges.get(config.partner, (err, badges) => {
  if (err) log.error(`Error (${err.message} getting badges for ${config.partner})`)

  let partnerIDs = []
  for (let badge of badges) {
    if (badge.appid) partnerIDs.push(badge.appid)
  }

  let ownerCards = {}
  inventory(config.owner, (err, items) => {
    if (err) log.error(err)

    for (let item of items) {
      let id = card.parse(item)
      if (id) {
        if (ownerCards[id]) { ownerCards[id].push(item) }
        else { ownerCards[id] = [item] }
      }
    }

    log.info(`parsed all item data and found trading cards for ${Object.keys(ownerCards).length} games.`)
    console.log(ownerCards)
  })
})
