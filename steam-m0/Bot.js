const EventEmitter = require('events')

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
    steam: client,
    domain: 'fsoc.space',
    language: 'en'
  })

  fwd(this.client, this)

  this.client.setOption('promptSteamGuardCode', false)
  this.client.logOn(account)

  this.client.on('steamGuard', (domain, callback) => {
    if (account.shasec) { require('steam-totp').getAuthCode(account.shasec, (e, code) => callback(code)) }
    else { callback(require('readline-sync').question(`${domain ? 'Email' : 'Mobile'} code: `)) }
  })

  this.client.on('webSession', (sessionID, cookies) => {
    this.web.setCookies(cookies)
    this.trader.setCookies(cookies)

    this.emit('ready')
  })
}
