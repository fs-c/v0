// THIS IS NOT FUNCTIONAL

const express = require('express')
const app = express()
const server = require('http').createServer(app)
const io = require('socket.io')(server)

const SteamCommunity = require('steamcommunity')

// TODO: Enforce limit of ~10 requests to steamcommunity.com per minute,
//       OR add clientside spam protection & socket connection limits.
let community = new SteamCommunity()

const PORT = 2904

server.listen(PORT, () => console.log('Server running on port ' + PORT))
app.use(express.static(__dirname + '/public/'))

let groups = {}
let connected = []

io.on('connection', socket => {
  console.log(`${socket.id.splice()} connected.`)
  connected.push(socket)

  socket.on('changeGroup', name => {
    if (socket[group]) {
      console.log(`Client already had group, removing.`)
      delete groups[name]
    } else { console.log(`No group associated with client, proceeding.`) }

    socket[group] = name

    if (!groups[name]) {
      groups[name] = null
      console.log(`changeGroup request success, added group ${name}.`)
    } else {
      console.log(`changeGroup request success, group ${name} already tracked.`)
    }
  })

  socket.on('disconnected', () => {
    connected.splice(connectSed.indexOf(socket), 1)
    console.log(`${socket.id} disconnected.`))
  }
})

function tick () {

}

function getGroup (name, callback) {
  community.getSteamGroup(name, (err, group) => {
    if (err) {
      console.log(`Error getting group ${group}.`)
      setTimeout(get, 60 * 1000, name, callback)
    } else {
      console.log(`Got got group ${name}.`)
      callback(group)
    }
  })
}
