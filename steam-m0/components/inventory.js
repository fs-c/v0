let Bot = require('../Bot')

Bot.prototype.initInventory = function () {
  this.community.getUserInventoryContents(this.steamID, 753, 6, true, (err, inv) => {
    if (err) return this.emit('communityError', err)
    this.inventory.steam = inv
    this.cards = this._getCards(this.inventory.steam)
    this.emit('inventoryReady')
  })
}
