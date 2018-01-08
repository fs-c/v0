#!/usr/bin/env node

global = { dictionary: {  } }

const User = require('steam-user')

const rl = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout,
  completer: (line, callback) => {
    const hits = Object.values(global.dictionary)
      .map(e => e.name).filter(e => e.startsWith(line))
        
    callback(null, [ hits, line ])
  }
}).on('line', require('./components/handleInput'))

const log = require('./components/logger')
const dictionary = require('./components/dictionary')

const retry = (fn, ...args) => fn(...args).catch(err => retry(fn, args))

let client = new User()
let account = require('./scripts/getAccount')()

client.setOption('promptSteamGuardCode', false)
require('./components/logEvents')(client)

client.logOn(account)

client.on('steamGuard', (email, callback) => {
  if (!email && account.shasec)
    return callback(require('steam-totp').getAuthCode(account.shasec))

  console.log(`${email ? 'Email' : 'Mobile'} code: `)
  rl.on('line', line => callback(line.trim()))
})

client.on('loggedOn', () => {
  client.setPersona(1)

  log.info('ready')

  retry(dictionary.build, client.steamID.getSteamID64())
    .then(dict => {
      log.debug(`got dictionary`)
    
      global.dictionary = dict
    }).catch(console.error)
})

client.on('friendMessage', (senderID, message) => {
  console.log(`${global.dictionary[senderID.toString()].name} >> ${message}`)
})