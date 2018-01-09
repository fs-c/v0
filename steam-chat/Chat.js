const User = require('./components/User')

const log = require('./components/logger')

const Chat = module.exports = class extends User {
  constructor(account) {
    super(account)

    this.logOn().then(() => {
      log.debug(`logged on`)

      // Go online to receive messages.
      client.setPersona(User.EPersonaState['Busy'])
    }).then(() => this.buildDictionary(this.client.steamID)).then(dict => {
      this.dictionary = dict
      this.emit('dictionary', this.dictionary)
    }).catch(log.error) // This should never throw.

    this.client.on('friendMessage', (senderID, content) => {
      let message = {
        content,
        sender: dictionary[senderID.toString()] || { id: senderID }
      }

      this.emit('message', message)
    })
  }

  send(recipient, message) {
    this.client.chatMessage(recipient, message || '')
  }
}

Chat.prototype.buildDictionary = require('./components/buildDictionary')