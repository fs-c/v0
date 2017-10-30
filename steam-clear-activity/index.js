const SteamUser = require('steam-user')
const steamtotp = require('steam-totp')

const DATA = require('../../steamdata')

function hide (client) {
  client.gamesPlayed([399220, 399080, 399480])
}

function login (client, account) {
  client.logOn({
    accountName: account.name,
    password: account.password
  })
}

function build (account) {
  let client = new SteamUser()

  client.setOption('promptSteamGuardCode', false)

  login(client, account)

  client.on('steamGuard', (domain, callback) => {
    steamtotp.getAuthCode(account.shasec, (err, code, offset, latency) => {
      if (err) console.error(err)
      callback(code)
    })
  })

  let timer
  client.on('loggedOn', details => {
    log(`Logged on from ${details.public_ip}.`)
    hide(client)
    timer = setInterval(hide, 2*60*1000, client)
  })

  client.on('error', err => {
    clearInterval(timer)

    console.error(err)

    if (err.message === 'LoggedInElsewhere') {
      setTimeout(
        function() { timer = setInterval(hide, 2*60*1000, client) },
        10*60*1000
      )
    } else {
      let i = (err.message === 'RateLimitExceeded' ? 30*60*1000 : 2*60*1000)
      client.logOff()
      setTimeout(login, i, client)
    }
  })
}

for (let name in DATA) { build(DATA[name]) }
