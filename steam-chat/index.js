const User = require('steam-user')

const log = require('./components/logger')
const account = require('./scripts/getAccount')()

let client = new User()

client.setOption('promptSteamGuardCode', false)

require('./components/logEvents')(client)

console.log(account)

client.logOn(account)

client.on('loggedOn', () => log.event('logged on'))