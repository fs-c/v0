const log = require('./logger')

const express = require('express')
let app = express()

app.enable('trust proxy')

const helmet = require('helmet')
app.use(helmet())

app.get('/', (req, res) => {
  log.verbose(`GET / served (${req.ip}).`)
  res.sendFile(__dirname + '/public/index.html')
})

const API = require('./api/ApiController')
app.use('/api', API)

const PORT = process.env.PORT || 8080
app.listen(PORT, () => log.info(`listening on port ${PORT}.`))
