const log = require('./logger')

const handleChat = input => {
  let recipient = input.slice(0, input.indexOf('<<')).trim()
  let message = input.slice(input.indexOf('<<') + 2).trim()

  console.log(global.dictionary)

  let id = Object.values(global.dictionary)
    .filter(e => e.customURL === recipient || e.name === recipient)[0] || {}
    .steamID

  if (!id) 
    return log.warn(`cannot find id`)
  
  log.debug(`sending message ${message} to ${recipient} / ${id.toString()}`)

  client.chatMessage(id, message)
}

const handle = module.exports = input => {
  if (input.indexOf('<<') !== -1)
    return handleChat(input)
}