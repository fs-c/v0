const colors = require('colors')

const time = () => require('moment')().format('hh:mm:SSA')

const config = {
  error: 'red',
  debug: 'blue',
  warn: 'yellow',
  info: 'green',
  chat: 'cyan',
  verbose: 'grey'
}

colors.setTheme(config)

const log = Object.keys(config).reduce((acc, val, i, arr) => {
  acc[val] = (msg, ...args) => {
    if (val === 'debug' && process.env.NODE_ENV !== 'dev') return

    console.log(`${time()} ${val[val]} - ${msg}`)
    
    if (process.env.NODE_ENV === 'dev' && process.argv.includes('--verbose'))
      args[0] && console.log(args)
  }

  return acc
}, {  })

module.exports = log