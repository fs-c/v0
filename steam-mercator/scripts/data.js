const PATH = process.env.SET_DATA || './set_data'

const fs = require('fs')
const request = require('request')
const log = require('winston')

exports.sets = getSets
exports.parse = parseSets

function getSets () {
  function get () {
    return new Promise((resolve, reject) => {
      request('http://cdn.steam.tools/data/set_data.json', (err, res, body) => {
        log.silly(`steam.tools CDN GET: ${res.statusCode}`)
        if (err || res.statusCode !== 200) reject(new Error(err ? err : res.statusCode))

        let b = JSON.parse(body)
        b.updated = Date.now()

        fs.writeFileSync('set_data.json', JSON.stringify(b))
        log.silly(`set_data.json written.`)
        resolve(JSON.parse(b))
      })
    })
  }

  return new Promise((resolve, reject) => {
    if (fs.existsSync(PATH)) {
      let d = require(PATH)
      log.silly(`set_data.json found.`)
      if (d.updated + 6*60*60*1000 > Date.now()) {
        resolve(d)
      } else {
        log.silly(`set_data.json file too old, fetching updated version.`)
        get().then(data => resolve(data)).catch(err => reject(err))
      }
    } else {
      get().then(data => resolve(data)).catch(err => reject(err))
    }
  })
}

function parseSets (data) {
  return new Promise((resolve, reject) => {
    let res = []
    for (let set of data) {
      res.push({
        count: set.normal.count
        appid: set.appid,
        game: set.game
      })
    }
    resolve(res)
  })
}
