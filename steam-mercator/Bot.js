const log = require('winston')

const Steam = require('steam-user')
const Trader = require('steam-tradeoffer-manager')

module.exports = Bot

function Bot (client) {
  this._client = client
  this._manager = new Trader({
    steam: client,
    domain: 'fsoc.space',
    language: 'en'
  })

  this.inventory = {}

  this._client.setPersona(Steam.EPersonaState.Online)

  this._client.on('friendMessage', (steamID, message) => {
    log.silly(`friendMessage event received from ${steamID.toString()} (${message.length > 25 ? message.slice(0, 25) + '...' : message}).`)
  })

  this._client.on('friendRelationship', (steamID, rel) => {
    log.silly(`friendRelationship with ${steamID.toString()} changed: ${Steam.EFriendRelationship[rel]}`)
    if (rel === Steam.EFriendRelationship.RequestRecipient) {
      this._client.addFriend(steamID, (err, name) => {
        log.silly(`accepted request of ${steamID.toString()} (${name})`)
      })
    }
  })

  // Only gets called on fatal disconnect so just allow 'graceful' crash.
  client.on('error', err => log.error(`bot encountered fatal error: ${err.message || err}`))
}
