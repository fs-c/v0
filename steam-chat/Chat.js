const User = require('./components/User')

const log = require('./components/logger')

const Chat = module.exports = class extends User {
  constructor(account) {
    super(account)

    this.logOn().then(() => {
      return log.debug(`logged on`)
    }).then(() => this.buildDictionary(this.client.steamID)).then(dict => {
      this.dictionary = dict
      this.emit('dictionary', this.dictionary)
    }).catch(log.error)

    this.client.on('friendMessage', (senderID, content) => {
      let message = {
        content,
        sender: dictionary[senderID.toString()] || { id: senderID }
      }

      this.emit('message', message)
    })
  }

  send(recipient, message) {
    this.client.chatMessage(recipient, message)
  }
}

Chat.prototype.buildDictionary = require('./components/buildDictionary')