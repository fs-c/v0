const log = require('winston')

const Steam = require('steam-user')
const Trader = require('steam-tradeoffer-manager')
const Community = require('steamcommunity')

const EventEmitter = require('events')

const spam = require('./scripts/spam')
const parse = require('./scripts/parse')

// TODO: usage of util.inherits() is discouraged by nodejs.
require('util').inherits(Bot, EventEmitter)

module.exports = Bot

function Bot (client) {
  EventEmitter.call(this)

  this._client = client
  this._community = new Community()
  this._manager = new Trader({
    steam: client,
    domain: 'fsoc.space',
    language: 'en'
  })

  this._community.setCookies(this._client._webSession.cookies)

  this.user = {}
  this.ready = false
  this.steamID = this._client.steamID || this._community.steamID

  this._client.setPersona(Steam.EPersonaState.Online)

  this._client.on('friendMessage', (steamID, message) => {
    log.silly(`friendMessage event received from ${steamID.toString()} (${message.length > 25 ? message.slice(0, 25) + '...' : message}).`)

    if (!spam(steamID.toString(), message)) {
      if (this.ready) {
        let inp = parse(message)
        if (inp) {
          log.verbose(`parsed message, `, inp)
          this.emit('cmd', inp)
        } else this._client.chatMessage(steamID, `Couldn't parse input.`)
      } else this._client.chatMessage(steamID, `Bot not ready yet.`)
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

require('./components/community')
