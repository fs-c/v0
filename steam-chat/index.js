const Chat = require('./Chat')

const log = require('./components/logger')

const readline = require('readline')

// Prepare global object with config, this throws if it misses something that
// can't be prompted..
require('./scripts/getConfig')

let chat = new Chat(global.ACCOUNT)

// Setup global radline interface with autocompletion.
global.rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  completer: (line, callback) => {
    if (!chat.dictionary) // Fallback to empty array if not yet initialised.
      return callback(null, [ [], line ])

    const hits = Object.values(chat.dictionary)
      .map(e => e.name).filter(e => e.startsWith(line))

    callback(null, [ hits, line ])
  }
})

// The last (newest) message message we received. 
let lastReceived = {  }

chat.on('dictionary', dict => {
  log.debug(`built dictionary`)
})

// Output incoming messages and update last received message.
chat.on('message', message => {
  log.debug(`received message`, message)

  lastReceived = message

  console.log(`[${message.formattedDate}]`
    + `${message.sender.name || message.sender.id.toString()} > `
    + `${message.content}`)
})

// Handle and parse user input.
rl.on('line', input => {
  if (global.rl.configListen || global.rl.codeListen)
    return // handled by other listeners

  if (input.includes('/'))
    return // TODO: handle command

  // Either:
  //  'recipient > message' or
  //  '^ message' where recipient is sender of last received message.
  let message = input.slice(input.indexOf(input.includes('<') ? '<' : '^') + 1)
  let recipient = input.includes('<')
    ? chat.dictionary[input.slice(0, input.indexOf('<')).trim()]
    : input.includes('^') 
      ? lastReceived.author ? lastReceived.author.id : undefined
      : undefined

  if (recipient)
    return log.warn(`invalid recipient`)

  return chat.send(recipient, message)
})