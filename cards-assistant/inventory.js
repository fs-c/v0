const log = require('winston')

const SteamCommunity = require('steamcommunity')
let community = new SteamCommunity()

module.exports = {
  get (id, callback) {

    id = new (SteamCommunity.SteamID)(id)
    community.getSteamUser(id, (err, user) => {
      if (err) {
        callback(err)
        return
      }
      log.verbose(`got profile of ${id}.`)

      // Steam= 753, Community contextID = 6, tradeable only
      user.getInventoryContents(753, 6, true, english, (err, inv, c, total) => {
        if (err) {
          callback(err)
          return
        }
        log.verbose(`got ${total} items from ${user.name}'s inventory.`)
      })
    })
  }
}
