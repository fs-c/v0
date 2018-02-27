const mongoose = require('mongoose')

module.exports = global.IDLER_DB.model('Account', new mongoose.Schema({
  name: String,
  pass: String,
  shasec: String,
  owner: String
}))
