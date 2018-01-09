const EventEmitter = require('events')
const SteamUser = require('steam-user')

const log = require('./logger')

const steamTotp = require('steam-totp')

const User = module.exports = class extends EventEmitter {
  constructor(account) {
    super()

    this.account = account

    this.client = new SteamUser()
    this.client.setOption('promptSteamGuardCode', false)
  }

  logOn() {
    log.debug(`logging on with account ${this.account.accountName}`)

    // Only errors relating to incorrect account data are considered fatal.
    const isFatal = err => {
      switch (err.eresult) {
        case 5, 12, 13, 15, 18: return true
        default: return false
      }
    }

    this.client.logOn(this.account)

    this.client.on('steamGuard', (email, callback) => {
      log.debug(`got steamGuard event (${email ? 'email' : 'mobile'})`)

      if (!email && this.account.shasec)
        return callback(steamTotp.generateAuthCode(this.account.shasec))

      global.rl.codeListen = true
      console.log(`Enter ${email ? 'email' : 'mobile'} code: `)
      global.rl.on('line', input => {
        if (!global.rl.codeListen) return
        callback(input.slice(5).trim())
        global.rl.codeListen = false
      })
    })

    return new Promise((resolve, reject) => {
      // Resolve once we get a session, not only on loggedIn.
      this.client.on('webSession', () => resolve())

      // Only reject on fatal error, retry otherwise.
      this.client.on('error', err => {
        log.debug(`got error ${SteamUser.EResult[err.eresult]}`, err)

        if (isFatal(err))
          return reject(err)

        setTimeout(this.client.logOn, 30 * 1000, this.account)
      })
    })
  }
}