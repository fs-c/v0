const log = require('winston')

const SteamCommunity = require('steamcommunity')
let community = new SteamCommunity()

module.exports = function get (id, callback) {
  id = new (SteamCommunity.SteamID)(id)
  community.getSteamUser(id, (err, user) => {
    if (err) {
      callback(err)
      return
    }
    log.info(`got profile of ${id}.`)

    // Steam = 753, Community contextID = 6, tradeable only
    user.getInventoryContents(753, 6, true, (err, inv, c, total) => {
      if (err) {
        callback(err)
        return
      }
      log.info(`got ${total} items from ${id}'s inventory.`)
      callback(null, inv)
    })
  })
}
