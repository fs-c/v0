const mongoose = require('mongoose')

const userSchema = new mongoose.Schema({
  username: String,
  password: String,
  access: { type: Number, default: 3 }
})

module.exports = mongoose.model('User', userSchema)