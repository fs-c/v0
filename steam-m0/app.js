const PATH = process.env.DATA_PATH || './accountdata'
const ENV  = process.env.NODE_ENV || 'prod'

const ACCOUNT = require(PATH).bot

const log = require('./logger')

const Bot = require('./Bot')
let bot = new Bot(ACCOUNT)

bot.on('ready', () => {
  log.info('bot logged on.')
  bot.setupHandlers()
})

// TODO: Figure out a way to listen to all events.
if (ENV === 'dev') {
  bot.on('loggedOn', () => log.debug(`loggedOn event by steam-user`))
  bot.on('steamGuard', () => log.debug(`steamGuard event by steam-user`))
  bot.on('disconnected', () => log.debug(`disconnected event by steam-user`))
  bot.on('sentry', () => log.debug(`sentry event by steam-user`))
  bot.on('webSession', () => log.debug(`webSession event by steam-user`))
  bot.on('loginKey', () => log.debug(`loginKey event by steam-user`))

  bot.on('friendMessage', (sid, msg) =>
    log.debug(`friendMessage event by steam-user (${sid.toString()}: ${msg.length > 25 ? msg.slice(0, 25) + '...' : msg})`))

  bot.on('relChange', (sid, rel) =>
    log.debug(`relationship with ${sid} is now ${rel}`))

  bot.on('cmd', (cmd, arg) =>
    log.debug(`command ${cmd} ${arg ? `with arguments ${arg} ` : ``}received.`))

  bot.on('spamMessage', sid => log.debug(`spam by ${sid.toString()} ignored.`))

  bot.on('steamUserError', err =>
    log.error(`something went wrong with the bot, retrying in 10 minutes. (${err.msg || err.message || err})`))
}
