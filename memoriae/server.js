const express = require('express')
const app = express()

app.use(require('helmet')())
app.use(require('morgan')('dev', {
  stream: { write: msg => require('./logger').verbose(msg.trim()) }
}))

app.get('*', express.static('app/public'))

app.listen(8080)