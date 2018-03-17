const URL = 'http://api.steampowered.com/'
const KEY = process.env.API_KEY || require('../../../steamdata.json').main.apikey

const request = require('request')

const log = require('./logger')

const getFriends = module.exports = id => {
  return new Promise((resolve, reject) => {
    request(`${URL}ISteamUser/GetFriendList/v0001/` +
      `?key=${KEY}&steamid=${id}&relationship=friend`, 
      (err, res, body) => {
        if (err) 
          return reject(err)

        log.debug(`got friend list of user ${id}`)

        resolve(JSON.parse(body).friendslist.friends)
      }
    )
  })
}