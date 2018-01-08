const Community = require('steamcommunity')
const SteamID = Community.SteamID

const community = new Community()

const log = require('./logger')

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

const build = exports.build = id => {
  return new Promise((resolve, reject) => {
    require('./getFriends')(id)
      .then(expand).then(users => {
        resolve(users.reduce((acc, val) => {
          acc[val.steamID.toString()] = val
          return acc
        }, {  }))
      }).catch(reject)
  })
}