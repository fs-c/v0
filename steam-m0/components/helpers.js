let Bot = require('../Bot')

Bot.prototype._getCards = function (inv) {
  let cards = {}
  for (let item of inv) {
    let appid = item.market_fee_app
    if (!blacklist.includes(appid) && item.market_name.indexOf('Foil') === -1) {
      if (!cards[appid]) cards[appid] = [ item.market_name ]
      else cards[appid].push(item.market_name)
    }
  }
  return cards
}
