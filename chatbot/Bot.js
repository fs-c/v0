const log = require('winston')

const CFG = require('./config')

const Steam = require('steam-user')

module.exports = function (client) {
  return new Promise((resolve, reject) => {
    client.setPersona(Steam.EPersonaState.Online)

    client.on('friendMessage', (steamID, msg) => {
      log.verbose(`message from ${steamID.getSteam3RenderedID()}: ${msg}`)
      client.chatMessage(steamID, CFG.defaults['chatResponse'])
    })

    client.on('friendRelationship', (steamID, rel) => {
      if (rel === Steam.EFriendRelationship.RequestRecipient) {
        log.verbose(`user ${steamID.getSteam3RenderedID()} sent a friend request.`)
        client.addFriend(steamID, err => {
          if (!err) client.chatMessage(steamID, CFG.defaults['welcomeMessage'])
        })
      } else {
        log.verbose(`relationship change with user ${steamID.getSteam3RenderedID()}, changed to ${rel}`)
      }
    })

    resolve()
  })
}
