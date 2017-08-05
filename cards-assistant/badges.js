const log = require('winston')
const request = require('request')

const URL = 'https://api.steampowered.com/IPlayerService/GetBadges/v1/'
let KEY = require('./config').apikey

// Development
if (!KEY) KEY = require('../../steamdata.json').main.apikey

module.exports = {
  get (id, callback) {
    log.verbose(`getting badges of ${id}.`)

    // TODO: Move from callback to promise system.
    request(`${URL}?key=${KEY}&steamid=${id}&format=json`, (err, res, body) => {
      if (err || res.statusCode !== 200) {
        callback(err ? err : res.statusCode)
        return
      }

      let badges = JSON.parse(body).response.badges
      log.info(`retrieved ${badges.length} badges from ${id}.`)

      callback(null, badges)
    })
  }
}
