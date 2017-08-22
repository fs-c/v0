const C = require('steamcommunity')
let c = new C()

module.exports = Group

function Group (id) {
	this._id = new C.SteamID(id)
	this.members = []
}

Group.prototype.getMembers = function () {
	return new Promise((resolve, reject) => {
		c.getGroupMembers(this._id, (err, members) => {
			if (err) { reject(err) } else {
				this.members = members
				resolve(members)
			}
		})
	})
}
