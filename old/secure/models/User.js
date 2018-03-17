const mongoose = require('mongoose')

const userSchema = new mongoose.Schema({
  username: String,
  password: String,
  access: { type: Number, default: 3 } // Access level; -1: root, 3: janitor.
})

module.exports = mongoose.model('User', userSchema)