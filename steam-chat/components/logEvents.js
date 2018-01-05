const Steam = require('steam-user') // For the enums.

const log = require('./logger')

module.exports = client => {
  client.on('loggedOn', details => 
    log.event(`loggedOn event`, details))

  client.on('steamGuard', email => 
    log.event(`steamGuard event${email ? ' email code needed' : ''}`))

  client.on('error', err => 
    log.warn(`error event (${Steam.EResult(err.eresult)})`))

  client.on('disconnected', (eresult, msg) =>
    log.warn(`disconnected event (${Steam.EResult(eresult)}${msg ? ', ' + msg : ''})`))

  client.on('webSession', () => 
    log.event(`webSession event received`))

  client.on('friendMessage', () => 
    log.event(`friendMessage event received`))

  client.on('friendTyping', () => 
    log.event(`friendTyping event received`))
}