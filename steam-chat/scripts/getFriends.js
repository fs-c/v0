const URL = 'http://api.steampowered.com/'

const request = require('request')

const log = require('../components/logger')

const getFriends = module.exports = id => {
  return new Promise((resolve, reject) => {
    request(`${URL}ISteamUser/GetFriendList/v0001/` +
      `?key=${global.API_KEY}&steamid=${id}&relationship=friend`, 
      (err, res, body) => {
        if (err) 
          return reject(err)

        log.debug(`got friend list of user ${id}`)

        resolve(JSON.parse(body).friendslist.friends)
      }
    )
  })
}