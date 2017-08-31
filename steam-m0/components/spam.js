let Bot = require('../Bot')

const INTERVAL = (process.env.NODE_ENV === 'dev' ? 0 : (process.env.SPAM_INTERVAL || 10)) * 1000

// TODO: Replace suboptimal spam check.
let m = {}
Bot.prototype.spam = function (steamID, message) {
	let s = m[steamID] ? m[steamID].time + INTERVAL < Date.now() ? false : true : false
  m[steamID] = { message, time: Date.now() }
  return s
}
