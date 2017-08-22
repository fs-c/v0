const C = require('steamcommunity')
let c = new C()

exports.get = get

function get (id) {
	return new Promise((resolve, reject) => {
		c.getSteamUser(id, (err, user) => {
			if (err) { reject(err) }
			else resolve(user)
		})
	})
}
