const Chat = require('./Chat')
const User = require('./User')

const log = require('./components/logger')

const readline = require('readline')

let rl = global.rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
})

let chat = {  } // Stub until user ready.
let user = new User(require('./scripts/getAccount')())

// The last (newest) message message we received. 
let lastReceived = {  }

// Log on to steam with given account and pass initialize chat on success.
user.logOn()
  .then(() => chat = new Chat(user.client))
  // This only throws when a truly fatal error occurs, no point in continuing. 
  .catch((err, ...info) => { throw log.error(err, ...info) })

// Set up readline only when we have a list for the autocomplete function.
chat.on('dictionary', dict => {
  rl = global.rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    completer: (line, callback) => {
      const hits = Object.values(chat.dictionary)
        .map(e => e.name).filter(e => e.startsWith(line))
  
      callback(null, [ hits, line ])
    }
  })
})

// Log incoming messages, and update last received message.
chat.on('message', message => {
  log.debug(message)

  lastReceived = message

  console.log(`[${message.formattedDate}] ${message.sender.name} > ${message}`)
})

// Handle and parse user input.
rl.on('line', input => {
  if (input.includes('/'))
    return // handle command

  let message = input.slice(input.indexOf(input.includes('>') ? '>' : '^') + 1)
  let recipient = input.includes('>')
    ? chat.dictionary[input.slice(0, input.indexOf('>')).trim()]
    : input.includes('^') 
      ? lastReceived.author ? lastReceived.author.id : undefined
      : undefined

  return chat.send(recipient, message)
})