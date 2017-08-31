let Bot = require('../Bot')

Bot.prototype.setupHandlers = function () {
	this.client.on('friendMessage', (steamID, message) => {
		if (!this.options.spamProtection || !this.spam(steamID, message)) {
			// TODO: Replace sub-par parse algorithm.
			if (message.indexOf('!') === 0) {
				this.emit('cmd', {
					command: message.indexOf(' ') !== -1 ? message.slice(1, message.indexOf(' ') + 1).trim() : message.slice(1),
		      arguments: message.indexOf(' ') !== -1 ? message.slice(message.indexOf(' ') + 1).split(' ') : false
				})
			} else this.client.chatMessage(sid, `Couldn't parse message.`)
		} else this.emit('spamMessage', steamID)
	})

	this.client.on('friendRelationship', (steamID, rel) => {
		this.emit('relChange', steamID, rel)
		if (rel === Steam.EFriendRelationship.RequestRecipient) {
			this.client.addFriend(steamID, (err, name) => {
				this.emit('addedFriend', steamID, name)
			})
		}
	})
}
