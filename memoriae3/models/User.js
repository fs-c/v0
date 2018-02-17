const mongoose = require('mongoose')

const personSchema = new mongoose.Schema({
  name: String,
  phone: String,
  email: String,
  birth: String,
  notes: String
})

const userSchema = new mongoose.Schema({
  facebook: {
    id: String,
    token: String,
    email: String,
    name: String,
    username: String,
  },
  persons: [ personSchema ]
})

module.exports = mongoose.model('User', userSchema)