const express = require('express')
const app = express()
const server = require('http').createServer(app)
const io = require('socket.io')(server)

require('console-stamp')(console, 'HH:MM:ss')

const PORT = 2904

server.listen(PORT, () => console.log('Server running on port ' + PORT))
app.use(express.static(__dirname + '/public/'))

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
