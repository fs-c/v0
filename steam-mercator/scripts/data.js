const PATH = process.env.SET_DATA || './set_data'

const fs = require('fs')
const request = require('request')
const log = require('winston')

exports.sets = getSets
exports.parse = parseSets

function getSets () {
  return new Promise((resolve, reject) => {
    if (fs.existsSync(PATH)) {
      log.silly(`set_data.json found.`)
      resolve(require(PATH)) // Bypassing the JSON.parse size limit.
    } else {
      request('http://cdn.steam.tools/data/set_data.json', (err, res, body) => {
        log.silly(`steam.tools CDN GET: ${res.statusCode}`)
        if (err || res.statusCode !== 200) reject(Error(err ? err : res.statusCode))
        fs.writeFileSync('set_data.json', body)
        log.silly(`set_data.json written.`)
        resolve(JSON.parse(body))
      })
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
