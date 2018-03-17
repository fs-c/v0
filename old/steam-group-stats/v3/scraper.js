const http = require('request')

const SteamCommunity = require('steamcommunity')
let community = new SteamCommunity()

require('console-stamp')(console, 'HH:MM:ss')

let data

module.exports = {
  function get() { return data }
}
