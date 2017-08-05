const log = require('./logger')

const config = require('./config')
const badges = require('./badges')
const inventory = require('./inventory')

badges.get(config.partner, (err, badges) => {
  if (err) log.error(`Error (${err.message} getting badges for ${config.partner})`)

  log.verbose(`parsing data.`)
  let ids = []
  for (let badge of badges) {
    if (badge.appid) ids.push(badge.appid)
  }

  inventory.get(config.owner, (err) => {
    if (err) log.error(err)
  })
})
