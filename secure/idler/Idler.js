const EventEmitter = require('events')
const fwd = require('fwd')

const User = require('steam-user')
const totp = require('steam-totp')

const crypto = require('crypto')

const getGames = require('./scripts/getGames')
const log = require('../logger')

class Client extends EventEmitter {
  constructor(account) {
    super()

    this.user = new User()    
    this.account = account

    this.user.setOption('promptSteamGuardCode', false)

    this.timers = [  ]

    // Forward steam-user events to Client.
    fwd(this.user, this)
  }

  login() {
    const decipher = crypto.createDecipher('aes192', global.CRYPTO_KEY)

    log.debug(`logging on`)

    if (this.account.shasec)
      this.account.twoFactorCode = totp.getAuthCode(this.account.shasec)

    let decrypted = decipher.update(this.account.pass, 'hex', 'utf8')
    decrypted += decipher.final('utf8')
    
    this.user.logOn({
      accountName: this.account.name,
      password: decrypted,
      twoFactorCode: this.account.twoFactorCode
    })

    decrypted = null

    return new Promise((resolve, reject) => {
      this.user.on('error', err => {
        reject(err)
      })

      this.user.on('loggedOn', () => resolve())
      this.user.on('webSession', () => log.debug('got session'))

      let tried = false

      this.user.on('steamGuard', (domain, cb) => {
        log.debug(`got steamGuard event`)

        if (domain && !tried) {
          log.debug(`trying with code ${this.account.twoFactorCode}`)
          tried = true
          cb(this.account.twoFactorCode)
        }

        reject(new Error('code or shasec invalid'))
      })
    })
  }

  relog() {
    this.clearTimers()

    this.user.logOff()
    this.idle()
  }

  idle() {
    log.debug(`attempting idling`)

    this.login()
      .then(() => {
        log.info(`account ${this.account.name} logged on`)

        getGames(this.user.steamID.toString())
          .then(games => {
            log.debug(`got ${games.length} games`)

            this.user.setPersona(1)
            this.user.gamesPlayed([ 'nothing' ].concat(games))
          }).catch(err => log.warn(err))
      }).catch(err => {
        this.user.logOff()

        log.warn(`login failed (${err.message}) for ${this.account.name}`)
        log.debug(`retrying login for ${this.account.name} in 30 minutes`)

        this.timers.push(setTimeout(this.idle, 30 * 60 * 1000))

        return
      })
  }

  clearTimers() {
    for (const timer of this.timers)
      clearInterval(timer)
  }
}

const Idler = module.exports = class extends EventEmitter {
  constructor() {
    super()

    this.clients = {  }
  }

  add(accs) {
    for (const account of accs) {
      this.clients[account.name] = new Client(account)
      this.clients[account.name].idle()

      // Forward events of Client to Idler.
      fwd(this.clients[account.name], this)
    }
  }

  update(accs) {
    for (const account of accs) {
      if (this.clients[account.name]) {
        this.clients[account.name].account = account
        this.clients[account.name].relog()
      }
    }
  }
}