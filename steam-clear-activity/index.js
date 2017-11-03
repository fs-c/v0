const SteamUser = require('steam-user')
const steamtotp = require('steam-totp')

const DATA = require('../../steamdata')
const ACCOUNTS = [ 'main' ]

function hide (client) {
  console.log(`hide called`)
  client.gamesPlayed([])
  client.gamesPlayed([399220, 399080, 399480])
  client.gamesPlayed([])
}

function login (client, account) {
  console.log(`login called`)
  client.logOn({
    accountName: account.name,
    password: account.password
  })
}

function build (account) {
  console.log(`build called`)
  let client = new SteamUser()

  client.setOption('promptSteamGuardCode', false)

  login(client, account)

  client.on('steamGuard', (domain, callback) => {
    console.log(`steamGuard received`)
    steamtotp.getAuthCode(account.shasec, (err, code, offset, latency) => {
      if (err) console.error(err)
      callback(code)
    })
  })

  let timer
  client.on('loggedOn', details => {
    console.log(`loggedOn received`)
    hide(client)
    timer = setInterval(hide, 60 * 1000, client)
  })

  client.on('error', err => {
    clearInterval(timer)

    console.error('error: ' + err.message || err.msg || err) // Just in case.

    if (err.message === 'LoggedInElsewhere') {
      setTimeout(
        function() { timer = setInterval(hide, 2 * 60 * 1000, client) },
        10*60*1000
      )
    } else {
      let i = (err.message === 'RateLimitExceeded' ? 2 * 60 * 60 * 1000 : 2 * 60 * 1000)
      client.logOff()
      setTimeout(login, i, client)
    }
  })
}

for (let name in DATA) { if (ACCOUNTS.includes(name)) build(DATA[name]) }
