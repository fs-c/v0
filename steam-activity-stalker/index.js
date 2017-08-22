require('console-stamp')(console, 'HH:MM:ss')

const fs = require('fs')

const Group = require('./group')
const GID = '103582791437945007'

const User = require('./user')

let group = new Group(GID)
let queue = []

let data = require('./data')

let c = 0

group.getMembers()
.then(members => {
	console.log(`got ${members.length} members.`)
	queue = members
	return
})
.catch(err => console.error(err))

tick()
setInterval(tick, 10 * 1000)

function tick () {
	c++
	function log (msg) { console.log(`[${c}] ${msg}`) }

	let item = queue[0]
	if (!item) return log(`queue empty, skipping tick.`)

	queue.push(queue.shift())

	let i64 = item.toString()

	User.get(item)
	.then(user => {
		let o = user.onlineState
		let s = (o === 'in-game' ? 2 : o === 'online' ? 1 : 0)
		if (data[i64]) { data[i64].push(s) }
		else data[i64] = [ s ]

		log(`got user, online state: ${o} (${s})`)

		if (!(c % 6)) fs.writeFile('data.json', JSON.stringify(data), err => {
			if (err) { return log(`failed to write data.json: ${err}`) }
			else log(`wrote to data.json`)
		})
	})
	.catch(err => { return log(`error getting user ${i64}: ${err}`) })
}
