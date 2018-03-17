const path = require('path')

global.DEV = process.env.NODE_ENV === 'dev'
global.ROOT = global.DEV ? '/' : '/00/'
global.PATHS = {
  steam: path.join(require('os').homedir(), '.steam.json'),
  config: path.join(process.cwd(), '.config.json'),
  logger: path.join(process.cwd(), 'logger')
}

const MONGODB_PASS = process.env.DB_PASS ||
  require(global.PATHS.config).mongo_pass
const MONGODB_STRING = `mongodb://root:${MONGODB_PASS}`
  + `@ds247347.mlab.com:47347/fsoc-secure`

const log = require(global.PATHS.logger)

// Configure passport.
const passport = require('passport')
require('./config/passport')(passport)

// Set up server.
const express = require('express')
const app = express()

// Basic needs.
app.enable('trust proxy')
app.use(require('cookie-parser')())
app.use(require('body-parser').urlencoded({ extended: false }))

app.use(require('helmet')())

app.use(require('connect-flash')())

// Drop in replacement for bugged express-session.
app.use(require('cookie-session')({
  secret: 'secretive secret',
  resave: false,
  saveUninitialized: false 
}))

app.use(passport.initialize()) 
app.use(passport.session())

// Pass morgans logs to our own logger.
app.use(require('morgan')('dev', {
  stream: { write: msg => log.debug(msg.trim()) }
}))

app.set('views', __dirname + '/views')
app.set('view engine', 'ejs')

// Properly format json responses.
app.set('json spaces', 2)

// Set up global router.
app.use(require('./routes/router'))

// Serve CSS and such.
app.use(express.static('public'))

// Catch-all for errors.
app.use(require('./routes/error'))

app.listen(process.env.PORT || 8085)

// Configure and connect to database.
const mongoose = require('mongoose')
mongoose.promise = global.Promise

const db = mongoose.connection

mongoose.connect(MONGODB_STRING, {
  useMongoClient: true
})

db.on('error', log.error)
db.once('open', () => log.info('connected to db'))