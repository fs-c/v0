const log = require('winston')
const request = require('request')

const URL = 'https://api.steampowered.com/IPlayerService/GetBadges/v1/'
let KEY = require('./config').apikey

// Development
if (!KEY) KEY = require('../../steamdata.json').main.apikey

module.exports = {
  get (id, callback) {
    // TODO: Move from callback to promise system.
    request(`${URL}?key=${KEY}&steamid=${id}&format=json`, (err, res, body) => {
      if (err || res.statusCode !== 200) {
        callback(err ? err : res.statusCode)
        return
      }

      let badges = JSON.parse(body).response.badges
      log.verbose(`retrieved badges from ${id}.`)

      callback(null, badges)
    })
  }
}
