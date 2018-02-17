global.DEV = process.env.NODE_ENV === 'dev'

const MONGODB_STRING = global.DEV
  ? 'mongodb://root:admin@ds161306.mlab.com:61306/memoriae'
  : 'mongodb://root:' + process.env.PASS + '@ds161346.mlab.com:61346/memoriae_prod'

const path = require('path')
const express = require('express')
const mongoose = require('mongoose')
const passport = require('passport')

const app = express()

app.use(require('cookie-parser')())
app.use(require('body-parser').json())
app.use(require('body-parser').urlencoded({ extended: false }))
app.use(require('express-session')({
  secret: 'shhsecret',
  resave: true,
  saveUninitialized: true 
}))

app.use(passport.initialize())
app.use(passport.session())

app.use((req, res, next) => {
  require('./logger').silly(req.user)
  next()
})

require('./config/passport')(passport)

app.use(require('morgan')('dev', {
  stream: { write: msg => require('./logger').verbose(msg.trim()) }
}))

const isLoggedIn = (req, res, next) =>
  req.isAuthenticated() ? next() : res.redirect('/error')

app.use('/api', require('./routes/api'))
app.use('/auth', require('./routes/auth'))

// Serve react frontend.
app.use(express.static(path.join(__dirname, 'app', 'build')))
app.get('/', isLoggedIn, (req, res) => {
  res.sendFile(path.join(__dirname, 'app', 'build', 'index.html'))
})

// Catch-all for errors.
app.use(require('./routes/error'))

app.listen(process.env.PORT || 8080)

mongoose.promise = global.Promise
mongoose.connect(MONGODB_STRING, {
  useMongoClient: true
})

let db = mongoose.connection
db.on('error', require('./logger').error)
db.once('open', () => require('./logger').info('connected to db'))