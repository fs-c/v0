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

  let ownerIDs = []
  inventory(config.owner, (err, items) => {
    if (err) log.error(err)

    for (let item of items) {
      let id = card.parse(item)
      if (id && !ownerIDs.includes(id))
        ownerIDs.push(id)
    }

    log.info(`parsed all item data and found trading cards for ${ownerIDs.length} games.`)
  })
})
