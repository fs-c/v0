const log = require('winston')

const SteamCommunity = require('steamcommunity')
let community = new SteamCommunity()

const fs = require('fs')
const request = require('request')

module.exports = {
  load (id, callback) {
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
  },
  parse (inv, callback) {
    // I do this because I don't want to make calls to cdn.steam.tools too often.
    function getData (callback) {
      if (fs.existsSync('set_data.json')) {
        log.verbose(`set_data.json found, read data.`)
        callback(require('./set_data'))
      } else {
        log.verbose(`set_data.json not found, requesting data from steam.tools`)
        request('http://cdn.steam.tools/data/set_data.json', (err, res, body) => {
          if (err || !(res.statusCode === 200)) return
          callback(JSON.parse(body))
          fs.writeFileSync('set_data.json', body)
          log.verbose(`wrote set_data.json.`)
        })
      }
    }

    let sets = {}
    let count = 0
    getData(data => {
      log.warn(`processing ${data.game_count} sets, this might take a while and/or cause RAM problems.`)
      for (let set of data.sets) {
        if (inv[set.appid]) {
          log.silly(`found matching game (${set.appid})`)
          let unique = [...new Set(inv[set.appid])]

          if (set.true_count == unique.length) {
            count++
            log.silly(`found full set(s) for game ${set.appid}, ${unique.length}/${set.true_count}`)
            let set_count = Math.floor(inv[set.appid].length / set.true_count)
            for (let i = 0; i++ >= set_count;) {
              if (!sets[set.appid]) {
                sets[set.appid] = unique
              } else sets[set.appid].push(unique)
            }
          } else log.silly(`found no full set for game ${set.appid}, ${unique.length}/${set.true_count}`)
        }
      }

      log.verbose(`found ${count} uncrafted sets.`)
      callback(sets)
    })
  }
}
