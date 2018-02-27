const Steam = require('steam-user')
const Trader = require('steam-tradeoffer-manager')

const totp = require('steam-totp')
const log = require(global.PATHS.logger)

const Client = module.exports = class Client {
  constructor(account) {
    this.steam = new Steam()
    this.manager = new Trader({
      steam: this.steam,
      domain: 'fsoc.space',
      language: 'en',
      pollInterval: 5000
    })

    this.ready = false

    this.account = account
    this.account.twoFactorCode = totp.getAuthCode(account.shasec)

    this.steam.logOn(this.account)

    this.steam.on('loggedOn', () => log.debug('logged on'))
    
    this.steam.on('webSession', (session, cookies) => {
      this.manager.setCookies(cookies, err => {
        if (err) return log.error(err) // If this fails we are basically fucked.
        
        // We are now logged in to steam and ready to send and receive trades.
        this.ready = true
      })
    })
    
    this.steam.on('error', err => {
      log.debug(`encountered error ${err.message} (${err.enum})`)

      // Simply retry on error.
      setInterval(this.steam.logOn, 30 * 1000, this.account)
    })
  }

  static get EOfferFilter() { return Trader.EOfferFilter }
}

require('./getOffers')