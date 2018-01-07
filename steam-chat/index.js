#!/usr/bin/env node

const User = require('steam-user')

let dictionary = {  }

const rl = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout,
  completer: (line, callback) => {
    const hits = Object.values(dictionary)
      .map(e => e.name).filter(e => e.startsWith(line))
        
    callback(null, [ hits, line ])
  }
})

const log = require('./components/logger')
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

  retry(require('./components/buildDictionary'), client.steamID.getSteamID64())
    .then(dict => {
      log.debug(`got dictionary`)
    
      dictionary = dict
    }).catch(console.error)
})

client.on('friendMessage', (senderID, message) => {
  console.log(`${dictionary[senderID.toString()].name} >> ${message}`)
})

rl.on('line', input => {
  if (input.indexOf('<<') === -1) return

  let recipient = input.slice(0, input.indexOf('<<')).trim()
  let message = input.slice(input.indexOf('<<') + 2).trim()

  let id = Object.values(dictionary)
    .filter(e => e.customURL === recipient || e.name === recipient)[0]
    .steamID

  log.debug(`sending message ${message} to ${recipient} / ${id.toString()}`)

  client.chatMessage(id, message)
})