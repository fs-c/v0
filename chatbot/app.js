/**
*   Accept all friend requests if users level > 5, else decline.
*   Send welcome message to all new friends.
*   Reply to all incoming messages with a set mesasage.
*/

const ACCOUNT = process.env.NODE_ENV === 'dev' ? require('../../steamdata').stifler : {
  accountName: "",
  password: ""
}

const steam = require('./steam')
const Bot = require('./Bot')
const log = require('./logger')

steam.logOn(ACCOUNT)
  .catch(err => {
    log.error(`bot encountered error ${err.message} while logging in.`)
  })
  .then(client => {
    log.info(`bot logged on.`)
    return new Bot(client)
  })
  .then(() => {
    log.info(`bot is ready.`)
  })
