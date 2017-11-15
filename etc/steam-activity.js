// By default requires a JSON file with your steam account data,
// as defined with the PATH variable.
// You can just define the accounts object yourself, just make sure
// to use the correct property names.

require('console-stamp')(console, 'HH:MM:ss.l')

const SteamUser = require('steam-user')
const steamtotp = require('steam-totp')

const fs = require('fs')
const rs = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
})

const PATH = '../steamdata.json'

let data
if (fs.existsSync(PATH)) {
  data = JSON.parse(fs.readFileSync(PATH))
} else throw new Error('Steam account data file not found.')

function hide (client) {
  client.gamesPlayed([399220, 399080, 399480])
  console.log(`Attempted to hide recent games.`)
}

function login (client, account) {
  client.logOn({
    accountName: account.name,
    password: account.password
  })
}

function build (account) {
  function log (msg) { console.log(`[${account.name}] ${msg}`) }

  let client = new SteamUser()

  client.setOption('promptSteamGuardCode', false)

  login(client, account)

  client.on('steamGuard', (domain, callback) => {
    log(`steamGuard event received.`)
    if (account.shasec) {
      steamtotp.getAuthCode(account.shasec, (err, code, offset, latency) => {
        if (err) throw err
        log(`Got mobile auth code (${code}) with a delay of ${latency} ms.`)
        callback(code)
      })
    } else {
      rs.question(`[${account.name}] ${domain ? 'Email' : 'Mobile'} code: `, code => callback(code))
    }
  })

  let timer
  client.on('loggedOn', details => {
    log(`Logged on from ${details.public_ip}.`)
    hide(client)
    timer = setInterval(hide, 2 * 60 * 1000, client)
  })

  client.on('error', err => {
    clearInterval(timer)
    let i = (err.message === 'RateLimitExceeded' ? 30 * 60 * 1000 : 2 * 60 * 1000)
    log(`Error '${err.message}' catched, retrying in ${(i / 1000) / 60} minutes.`)

    if (err.message === 'LoggedInElsewhere') {
      setTimeout(
        function () { timer = setInterval(hide, 2 * 60 * 1000, client) },
        10 * 60 * 1000
      )
    } else {
      client.logOff()
      setTimeout(login, i, client)
    }
  })
}

for (let name in data) { build(data[name]) }
