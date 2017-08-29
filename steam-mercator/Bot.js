const log = require('winston')

const Steam = require('steam-user')
const Trader = require('steam-tradeoffer-manager')

const spam = require('./scripts/spam')
const parse = require('./scripts/parse')

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

    if (!spam(steamID.toString(), message)) {
      let inp = parse(message)
      if (inp) {
        
      } else this._client.chatMessage(steamID, `Couldn't parse input.`)
    }
  })

  this._client.on('friendRelationship', (steamID, rel) => {
    log.silly(`friendRelationship with ${steamID.toString()} changed: ${Steam.EFriendRelationship[rel]}`)
    if (rel === Steam.EFriendRelationship.RequestRecipient) {
      this._client.addFriend(steamID, (err, name) => {
        log.silly(`accepted request of ${steamID.toString()} (${name})`)
      })
    }
  })

  // Allow the app to gracefully shit it's pants. 'error' event is only emitted if fatal.
  this._client.on('error', err =>
    log.error(`bot encountered fatal error: ${err.message || err}`))
}
