const express = require('express')
const mongoose = require('mongoose')

const app = express()

app.use(require('morgan')('dev'))
app.use(require('cors')())

mongoose.connect('mongodb://root:admin@ds161306.mlab.com:61306/memoriae')

app.use('/api', require('./routes/api'))

app.listen(process.env.PORT || 8080)