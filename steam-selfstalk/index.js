const DATA = require('../../steamdata.json').main
const EVENTS = [
  'playingState',
  'friendRelationship',
  'friendMessage'
]

const User = require('steam-user')
const client = new User()

client.setOption('promptSteamGuardCode', false)
client.logOn(DATA)
client.on('steamGuard', (d, cb) => {
  require('steam-totp').getAuthCode(DATA.shasec, code => cb(code))
})

let stash = []

for (let event of EVENTS) {
  client.on(event, (...data) => {
    stash.push({
      timestamp: Date.now(),
      event,
      data
    })
  })
}

setInterval(update, 30 * 60 * 1000)

function update () {
  require('../logger').verbose(`updating! ${stash.length} new items.`)

  if (stash.length === 0) return

  const data = require('./data.json')
  data.concat(stash)
  require('fs').writeFileSync('data.json', JSON.stringify(data))

  stash = []
}
