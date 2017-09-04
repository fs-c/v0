const log = require('winston')

const C = require('steamcommunity')
let cm = new C()

let cache = []

function cached (id) {
  for (let i in cache) if (i === id) return true
  return false
}

function User (id) {
  return new Promise((resolve, reject) => {
    if (id.length === 17) {
      if (cached(id)) {
        if ((cache[id].updated + 10*60*1000) > Date.now()) {
          log.debug(`got profile ${id} from cache.`)
          resolve(cache[id].user)
          return
        } else delete cache[id]
      }

      cm.getSteamUser(new (C.SteamID)(id), (err, user) => {
        if (err) {
          log.error(`error while getting steam profile for ${id}: ${err.message}`)
          reject('Something went wrong while getting steam profile.')
          return
        }

        log.debug(`got steam profile for ${id}.`)

        resolve(user)

        cache[id] = { user, updated: Date.now() }
        log.debug(`added user ${id} to cache.`)

        if (Object.keys(cache).length > 100) {
          for (let id of Object.keys(cache).slice(99)) {
            delete cache[id]
            log.debug(`removed user ${id} from cache.`)
          }
        }
      })
    } else reject(`No valid Steam64 ID provided.`)
  })
}

module.exports = User
