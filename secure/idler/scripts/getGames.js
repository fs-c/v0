const APIKEY = require(global.PATHS.steam).default.apikey

const request = require('request')

const getGames = module.exports = id => {
  return new Promise((resolve, reject) => {
    request(`http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/`
      + `?key=${APIKEY}&steamid=${id}&format=json`, 
    (err, r, body) => {
      body = JSON.parse(body)

      resolve(body.response.games.map(e => e.appid))
    })
  })
}