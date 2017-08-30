const PATH = process.env.DATA_PATH || './accountdata'
const ENV  = process.env.NODE_ENV || 'prod'

const ACCOUNT = require(PATH).bot

const log = require('./logger')

const Bot = require('./Bot')
let bot = new Bot(ACCOUNT)

bot.on('ready', () => {
  log.info('bot ready.')
})

// TODO: Figure out a way to listen to all events.
if (ENV === 'dev') {
  bot.on('loggedOn', () => log.debug(`loggedOn event by steam-user`))
  bot.on('steamGuard', () => log.debug(`steamGuard event by steam-user`))
  bot.on('disconnected', () => log.debug(`disconnected event by steam-user`))
  bot.on('sentry', () => log.debug(`sentry event by steam-user`))
  bot.on('webSession', () => log.debug(`webSession event by steam-user`))
  bot.on('loginKey', () => log.debug(`loginKey event by steam-user`))
  bot.on('tradeOffers', () => log.debug(`tradeOffers event by steam-user`))
}
