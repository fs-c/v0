const log = require('../logger')

const User = require('steam-user')
const user = new User()

const account = {
  accountName: 'mrlaurinator'
}

const parse = string => string.split('\n').map(e => e.replace('\r', ''))
const keys = parse(require('fs').readFileSync('./keys.txt', 'utf8'))

const callback = (result, details, packages) => {
  log.debug(`result: ${User.EResult[result]}/${result} details: ${User.EPurchaseResult[details]}/${details}`)

  if (result === User.ERsult.OK) log.info(`successfully redeemed key`)
}

user.logOn(account)

user.on('loggedOn', () => {
  let delay = 0
  keys.map(key => setTimeout(user.redeemKey, delay++ * 30000, key, callback))
})
