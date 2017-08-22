const log = require('winston')

// const DEV = process.env.NODE_ENV === 'dev'
// log.warn(`process env DEV set to ${DEV}`)

const express = require('express')
let app = express()

const https = require('https')
const fs = require('fs')

// let server
// 
// if (!DEV) {
//   server = https.createServer(
//     {
//       key: fs.readFileSync('./tls/key.pem'),
//       cert: fs.readFileSync('./tls/cert.pem')
//     }, app
//   )
// }
//
// const PORTS = [
//   80, 443
// ]
//
// if (!DEV) server.listen(PORTS[1])

const CommentController = require('./comments/CommentController')
app.use('/comments', CommentController)

const UserController = require('./users/UserController')
app.use('/users', UserController)

app.get('/', (req, res) => {
  res.status(200).send(`Hello, friend.`)
  log.info(`apex index request from ${req.ip}.`)
})

module.exports = app

const NAME = 'projectbluestreak'

const Group = require('./group/group')
let group = new Group(NAME)

const data = require('./group/data')

group.getComments(comments => handle(comments))
setInterval(group.getComments, 5*60*1000, comments => handle(comments))

function handle (cmts) {
  log.info(`found a total of ${cmts.length} comment(s).`)
  data.add(cmts)
}
