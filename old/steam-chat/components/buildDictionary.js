const Chat = require('../Chat')

const SteamID = require('steamid')

const log = require('./logger')
const getFriends = require('../scripts/getFriends')

const community = new (require('steamcommunity'))()

const getInfo = id => {
  return new Promise((resolve, reject) => {
    community.getSteamUser(new SteamID(id), (err, user) => {
      if (err) {
        log.debug(`failed to get profile for ${id}`, err)
        return resolve()
      }

      resolve({
        steamID: user.steamID,
        name: user.name,
        onlineState: user.onlineState,
        stateMessage: user.stateMessage,
        avatar: user.getAvatarURL(),
        customURL: '/' + user.customURL
      })
    })
  })
}

const expand = friends => {
  log.debug(`getting details of ${friends.length} profiles`)

  return new Promise((resolve, reject) => {
    Promise.all(friends.map(e => getInfo(e.steamid)))
      .then(resolve).catch(log.error)
  })
}

// Build an object of friends of user with id, and populate it with their data.
const build = module.exports = id => {
  return new Promise((resolve, reject) => {
    getFriends(id.toString())
      .then(expand).then(resolve).catch(log.error)
  })
}