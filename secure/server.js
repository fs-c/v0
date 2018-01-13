global.DEV = process.env.NODE_ENV !== 'production'

const MONGODB_STRING = `mongodb://root:${process.env.DB_PASS}`
  + `@ds247347.mlab.com:47347/fsoc-secure`

// Configure passport.
const passport = require('passport')
require('./config/passport')(passport)

// Set up server.
const express = require('express')
const app = express()

// Since HTML links will always have to go to /00/*.
if (process.env.NODE_ENV !== 'production')
  app.get('/00/*', (req, res) =>
    res.redirect('/' + req.originalUrl.slice(4)))

// Basic needs.
app.enable('trust proxy')
app.use(require('cookie-parser')())
app.use(require('body-parser').urlencoded({ extended: false }))

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
  stream: { write: msg => require('./logger').debug(msg.trim()) }
}))

app.set('views', __dirname + '/views')
app.set('view engine', 'ejs')

// Properly format json responses.
app.set('json spaces', 2)

// Set up routes.
require('./routes/')(app, passport)

// Serve CSS and such.
app.use(express.static('public'))

// Catch-all for errors.
app.use(require('./routes/error'))

app.listen('8085' || process.env.PORT)

// Configure database.
const mongoose = require('mongoose')
mongoose.promise = global.Promise

const db = mongoose.connection

mongoose.connect(MONGODB_STRING, {
  useMongoClient: true
})

db.on('error', require('./logger').error)
db.once('open', () => require('./logger').info('connected to db'))