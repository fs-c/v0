const log = require('./logger')

const express = require('express')
const app = express()

app.enable('trust proxy')

app.use(require('helmet')())            // Basic security.
app.use(require('cors')())              // Enable all CORS requests.
app.use(require('morgan')('dev', {    // Logging.
  stream: { write: msg => log.verbose(msg.trim()) }
}))

app.set('json spaces', 4)

app.get('/', (req, res) =>
  res.sendFile(require('path').join(__dirname, '/index.html')))

app.use('/mrrobot', require('./mrrobot/'))
app.use('/steam', require('./steam'))

const PORT = process.env.PORT || 8081
app.listen(PORT, () => console.log(`listening on port ${PORT}.`))
