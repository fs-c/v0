const Client = require('./')

const log = require(global.PATHS.logger)

const parseItems = items => {
  return items.map(item => ({
    id: item.id,
    assetid: item.assetid,
    contextid: item.contextid,
    appid: item.appid,
    classid: item.classid,
    instanceid: item.instanceid,
    name: item.name
  }))
}

const parseOffers = offers => {
  return offers.map(offer => ({
    id: offer.id,
    partner: offer.partner.toString(),
    message: offer.message,
    state: offer.state,
    created: offer.created,
    expires: offer.expires,
    itemsToGive: parseItems(offer.itemsToGive),
    itemsToReceive: parseItems(offer.itemsToReceive)
  }))
}

Client.prototype.getOffers = function (status = 'ActiveOnly') {
  return new Promise((resolve, reject) => {
    const gotOffers = (err, sent, received) => {
      if (err)
        return reject(err)

      resolve({ sent: parseOffers(sent), received: parseOffers(received) })
    }

    const attempt = () => {
      if (this.ready) {
        this.manager.getOffers(Client.EOfferFilter[status], gotOffers)
      } else setTimeout(attempt, 5 * 1000)
    }

    attempt()
  })
}