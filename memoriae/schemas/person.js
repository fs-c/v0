const mongoose = require('mongoose')

const personSchema = new mongoose.SchemaType({
  name: String,
  phone: String,
  email: String,
  birth: String,
  notes: String
})

const Person = mongoose.model('Person', personSchema)

module.exports = Person