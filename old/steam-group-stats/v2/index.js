const express = require('express')
const app = express()
const server = require('http').createServer(app)
const io = require('socket.io')(server)

require('console-stamp')(console, 'HH:MM:ss.l');

const PORT = 2904

server.listen(PORT, () => console.log('Server running on port ' + PORT))
app.use(express.static(__dirname + '/public/'))

const SteamCommunity = require('steamcommunity')
let community = new SteamCommunity()

const GROUP = 'projectbluestreak'

let connected = []

io.on('connection', socket => {
  socket['rid'] = socket.id.slice(0, 9)
  console.log(`${socket.rid} connected.`)
  connected.push(socket)

  socket.on('disconnected', () => {
    connected.splice(connected.indexOf(socket), 1)
    console.log(`${socket.rid} disconnected.`)
  })
})

setInterval(function () {
  community.getSteamGroup(GROUP, (err, group) => {
    if (err || !group) console.log(err ? err : `No group object.`)

    // console.log(`Got CSteamGroup object of group ${GROUP}.`)

    let stats = {
      members: group.members,
      online: group.membersOnline,
      ingame: group.membersInGame
    }

    io.emit('update', stats)
  })
}, 10 * 1000)
