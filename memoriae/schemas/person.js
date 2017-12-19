const mongoose = require('mongoose')

const Person = mongoose.model('Person', mongoose.Schema({
  name: String,
  phone: String,
  email: String,
  birth: String,
  notes: String
}))

module.exports = Person