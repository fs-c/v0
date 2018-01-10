const URL = 'http://api.steampowered.com/'

const request = require('request')

const log = require('../components/logger')

const getFriends = module.exports = id => {
  let url = `${URL}ISteamUser/GetFriendList/v0001/` +
    `?key=${global.API_KEY}&steamid=${id}&relationship=friend`

  return new Promise((resolve, reject) => {
    request(url, (err, res, body) => {
      if (err) 
        return reject(err)

      try {
        // This will cause an exception if key invalid or profile private.
        let friends = JSON.parse(body).friendslist.friends
        log.debug(`got friend list of user ${id}`)

        resolve(friends)
      } catch(e) {
        log.warn(`couldn't get friendslist, you should probably abort`)
        reject(e)
      }
    })
  })
}