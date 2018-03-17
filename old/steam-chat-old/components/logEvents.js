const Steam = require('steam-user') // For the enums.

const log = require('./logger')

module.exports = client => {
  client.on('loggedOn', details => 
    log.debug(`loggedOn event (${details.ip_country_code}, ${details.vanity_url}, ${details.public_ip})`))

  client.on('steamGuard', email => 
    log.debug(`steamGuard event${email ? ' email code needed' : ''}`))

  client.on('error', err => 
    log.warn(`error event (${Steam.EResult[err.eresult]})`))

  client.on('disconnected', (eresult, msg) =>
    log.warn(`disconnected event (${Steam.EResult[eresult]}${msg ? ', ' + msg : ''})`))

  client.on('webSession', () => 
    log.debug(`webSession event received`))

  client.on('friendMessage', () => 
    log.debug(`friendMessage event received`))

  client.on('friendTyping', () => 
    log.debug(`friendTyping event received`))
}