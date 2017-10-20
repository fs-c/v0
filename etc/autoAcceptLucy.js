const DATA = require(process.env.DATA_PATH || '../../steamdata')

const Trader = require('../../v1/steam-trade-assistant/Trader.js')
const log = require('../logger')

let trader = new Trader(DATA['main'])

// Basic logging.
trader.on('ready', () => log.info(`trader ready.`))
trader.on('clientError', (err, res) => log.error(`error while logging in: ${res || err}`))
trader.on('managerError', err => log.error(`trade manager error: ${err.message || err}`))

trader.on('newOffer', offer => {
  log.verbose(`new offer ${offer.id}.`)

  // If offer partner is lucy.
  if (offer.partner.toString() === '76561198073021519') {
    if (offer.itemsToGive.length > 1) return

    // If item is the karambit.
    if (offer.itemsToGive[0].id === '12368203069') {
      trader.accept(offer).then(() => log.info(`accepted offer by lucy for karambit.`))
    }
  }
})
