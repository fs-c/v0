const express = require('express')
const mongoose = require('mongoose')

const app = express()

app.use(require('body-parser').json())
app.use(require('morgan')('dev'))
app.use(require('cors')())

mongoose.promise = global.Promise

mongoose.connect('mongodb://root:admin@ds161306.mlab.com:61306/memoriae', {
  useMongoClient: true
})

const db = mongoose.connection

db.on('error', console.error)
db.once('open', () => console.log('connected to db!'))

app.use('/api', require('./routes/api'))

app.listen(process.env.PORT || 8080)