let Bot = require('../Bot')

Bot.prototype.getSteamUser = () => {
  console.log(this)
  return new Promise((resolve, reject) => {
    this._community.getSteamUser(self.steamID, (err, user) => {
      if (err) { reject(err) } else resolve(user)
    })
  })
}
