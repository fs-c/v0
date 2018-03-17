const User = require('./components/User')

const moment = require('moment')

const log = require('./components/logger')

const Chat = module.exports = class extends User {
  constructor(account) {
    super(account)

    this.logOn().then(() => {
      log.debug(`logged on`)

      // Go online to receive messages.
      this.client.setPersona(User.EPersonaState['Busy'])
    }).then(() => this.buildDictionary(this.client.steamID)).then(dict => {
      this.dictionary = dict
      this.emit('dictionary', this.dictionary)
    }).catch(log.error) // This should never throw.

    this.client.on('friendMessage', (senderID, content) => {
      let message = {
        content,
        sender: this.dictionary
          .filter(e =>
            e.steamID.toString() === senderID.toString())[0] ||
            { id: senderID },
        formattedDate: moment().format('h:mm:ss a')
      }

      this.emit('message', message)
    })
  }

  send(recipient, message) {
    this.client.chatMessage(recipient, message || '')
  }
}

Chat.prototype.buildDictionary = require('./components/buildDictionary')