const INTERVAL = (process.env.SPAM_INTERVAL || 10) * 1000

let m = {}

module.exports = spam

// TODO: Replace this with something more s o p h i s t i c a t e d.
function spam (id, msg) {
  let s = m[id] ? m[id].time + INTERVAL < Date.now() ? false : true : false
  m[id] = { msg, time: Date.now() }
  return s
}
