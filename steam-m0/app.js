const PATH = process.env.DATA_PATH || './accountdata'

const ACCOUNT = require(PATH).bot

const log = require('./logger')

const Bot = require('./Bot')
let bot = new Bot(ACCOUNT)

bot.on('initialized', () => {
  log.info('bot initialized.')
  bot.setupHandlers()
  bot.initInventory()
})

bot.on('inventoryReady' () => {
  log.info('fetched and parsed inventory.')
})

// TODO: Listen to all events.
if (process.env.NODE_ENV === 'dev') {
  // Notable node-steam-user events
  bot.on('loggedOn', () => log.debug(`loggedOn event by steam-user`))
  bot.on('steamGuard', () => log.debug(`steamGuard event by steam-user`))
  bot.on('disconnected', () => log.debug(`disconnected event by steam-user`))
  bot.on('sentry', () => log.debug(`sentry event by steam-user`))
  bot.on('webSession', () => log.debug(`webSession event by steam-user`))
  bot.on('loginKey', () => log.debug(`loginKey event by steam-user`))

  // Bot.js events
  bot.on('friendMessage', (sid, msg) =>
    log.debug(`friendMessage event by steam-user (${sid.toString()}: ${msg.length > 25 ? msg.slice(0, 24) + '...' : msg})`))

  bot.on('relChange', (sid, rel) =>
    log.debug(`relationship with ${sid} is now ${rel}`))

  bot.on('cmd', (cmd, arg) =>
    log.debug(`command ${cmd} ${arg ? `with arguments ${arg} ` : ``}received.`))

  bot.on('spamMessage', sid => log.debug(`spam by ${sid.toString()} ignored.`))

  bot.on('inventoryReady', () => log.debug(`bot inventory ready.`))

  bot.on('steamUserError', err =>
    log.error(`something went wrong with the bot, retrying in 10 minutes. (${err.msg || err.message || err})`))

  bot.on('communityError', err =>
    log.error(`something went wrong while communicating with steam, retrying in 10 minutes. (${err.msg || err.message || err})`))
}
