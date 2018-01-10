const EventEmitter = require('events')
const SteamUser = require('steam-user')

const log = require('./logger')

const fwd = require('fwd')
const steamTotp = require('steam-totp')

// steam-user wrapper.
const User = module.exports = class extends EventEmitter {
  constructor(account) {
    super()

    this.account = account

    this.client = new SteamUser()

    // We'll do that ourselves.
    this.client.setOption('promptSteamGuardCode', false)

    // Forward all client events to own emitter.
    fwd(this.client, this)
  }

  // Expose the enums we need.
  static get EResult() { return SteamUser.EResult }
  static get EPersonaState() { return SteamUser.EPersonaState }  

  logOn() {
    log.debug(`logging on`)

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
        callback(input.trim())
        global.rl.codeListen = false
      })
    })

    return new Promise((resolve, reject) => {
      // Unlike loggedOn, only emitted when we really are logged on.
      this.client.on('webSession', () => resolve())

      // Only reject on fatal error, retry otherwise.
      this.client.on('error', err => {
        log.debug(`got error ${User.EResult[err.eresult]}`, err)

        if (isFatal(err))
          return reject(err)

        client.logOff() // Just in case. 

        // Retry if not fatal (e.g. because steam is being buggy)
        setTimeout(this.client.logOn, 30 * 1000, this.account)
      })
    })
  }
}