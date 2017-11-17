const MAX_FILES = 100
const PORT = process.env.PORT || 8083

const log = require('../logger')

const fs = require('fs')

const express = require('express')
const app = express()

app.enable('trust proxy')

app.use(require('helmet')())        // Basic security.
app.use(require('morgan')('dev', {  // Logging.
  stream: { write: msg => log.verbose(msg.trim()) }
}))

if (!fs.existsSync('./files'))
  fs.mkdirSync('./files')

app.use(express.static('files'))
app.use(express.static('static'))

const fileUpload = require('express-fileupload')
app.use(fileUpload({ limits: { fileSize: 1024 * 1024 } })) // 1 MiB

const routes = require('./routes')
app.post('/upload', routes.upload)
app.get('/delete/:name', routes.delete)

app.listen(PORT)
