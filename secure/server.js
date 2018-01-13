const MONGODB_STRING = `mongodb://root:${process.env.DB_PASS}`
  + `@ds247347.mlab.com:47347/fsoc-secure`

// Configure passport.
const passport = require('passport')
require('./config/passport')(passport)

// Set up server.
const express = require('express')
const app = express()

if (process.env.NODE_ENV !== 'production')
  app.get('/00/*', (req, res) =>
    res.redirect('/' + req.originalUrl.slice(4)))

app.enable('trust proxy')
app.use(require('cookie-parser')())
// app.use(require('body-parser').json())
app.use(require('body-parser').urlencoded({ extended: false }))

app.use(require('cookie-session')({
  secret: 'secretive secret',
  resave: false,
  saveUninitialized: false 
}))

app.use(passport.initialize()) 
app.use(passport.session())

app.use(require('morgan')('dev', {
  stream: { write: msg => require('./logger').debug(msg.trim()) }
}))

app.set('views', __dirname + '/views')
app.set('view engine', 'ejs')

app.set('json spaces', 2)

require('./routes/')(app, passport)

app.use(express.static('public'))

app.use(require('./routes/error'))

app.listen('8085')

// Configure database.
const mongoose = require('mongoose')
mongoose.promise = global.Promise

const db = mongoose.connection

mongoose.connect(MONGODB_STRING, {
  useMongoClient: true
})

db.on('error', require('./logger').error)
db.once('open', () => require('./logger').info('connected to db'))