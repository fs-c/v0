const EventEmitter = require('events')

// TODO: use of util.inherits is discouraged by nodejs.
require('util').inherits(Bot, EventEmitter)

module.exports = Bot

const User = require('steam-user')
const Community = require('steamcommunity')
const Manager = require('steam-tradeoffer-manager')

const fwd = require('fwd')

function Bot (account) {
  EventEmitter.call(this)

  this.client = new User()
  this.web = new Community()
  this.trader = new Manager({
    steam: this.client,
    domain: 'fsoc.space',
    language: 'en'
  })

  // Forward all steam interface events to our emitter.
  fwd(this.client, this)
  fwd(this.web, this)
  fwd(this.trader, this)

  // We'll do that ourselves.
  this.client.setOption('promptSteamGuardCode', false)
  this.client.logOn(account)

  this.client.on('steamGuard', (domain, callback) => {
    if (account.shasec) { require('steam-totp').getAuthCode(account.shasec, (e, code) => callback(code)) }
    else { callback(require('readline-sync').question(`${domain ? 'Email' : 'Mobile'} code: `)) }
  })

  // Unlike loggedOn this only gets emitted when logOn really was successful.
  this.client.on('webSession', (sessionID, cookies) => {
    this.web.setCookies(cookies)
    this.trader.setCookies(cookies)

    this.emit('ready')
  })
}
