const SteamUser = require('steam-user')

const log = require('./components/logger')

const steamTotp = require('steam-totp')

class User {
  constructor() {
    this.account = account

    this.client = new SteamUser()
  }

  logOn() {
    // Only errors relating to incorrect account data are considered fatal.
    const isFatal = err => {
      switch (err.eresult) {
        case 5, 12, 13, 15, 18: return true
        default: return false
      }
    }

    this.client.logOn(this.account)

    this.client.on('steamGuard', (email, callback) => {
      if (!email && this.account.shasec)
        return callback(steamTotp.generateAuthCode(this.account.shasec))

      console.log(`Enter /code CODE: `)
      rl.on('line', input =>
        input.includes('/code') && callback(input.slice(5).trim()))
    })

    return new Promise((resolve, reject) => {
      // Resolve once we get a session, not only on loggedIn.
      this.client.on('webSession', () => resolve())

      // Only reject on fatal error, retry otherwise.
      this.client.on('error', err => {
        if (isFatal(err))
          return reject(err)

        setTimeout(30 * 1000, this.client.logOn, this.account)
      })
    })
  }
}

module.exports = User