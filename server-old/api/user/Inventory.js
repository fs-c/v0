const log = require('winston')

const C = require('steamcommunity')
let cm = new C()

function Inventory (id, appID, contextID) {
  return new Promise((resolve, reject) => {
    if (id.length === 17) {
      cm.getUserInventoryContents(id, appID, contextID, false, 'english', (err, inv) => {
        if (err) {
          log.error(`error while getting inventory for ${id}: ${err.message}`)
          reject('Something went wrong while getting user inventory.')
          return
        }

        log.debug(`got inventory for ${id} (${appID} / ${contextID}).`)

        resolve(inv)

        cache[id] = { inv, updated: Date.now() }
        log.debug(`added inventory of ${id} to cache.`)
      })
    } else reject(`No valid Steam64 ID provided.`)
  })
}

module.exports = Inventory
