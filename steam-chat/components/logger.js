const colors = require('colors')
const time = () => require('moment')().format('hh:mm:SSA')

const config = {
  error: 'red',
  warn: 'yellow',
  debug: 'blue' 
}

colors.setTheme(config)

const log = module.exports = Object.keys(config).reduce((acc, val, i, arr) => {
  acc[val] = (msg, ...args) => {
    if (val === 'debug' && !global.DEV)
      return
    
    if (typeof msg === 'object') {
      console.log(`${time()}` + `${val} - object`[val])
      console.log(msg)
      args.forEach(console.log)

      return
    }

    console.log(`${time()} ${val[val]} - ${msg}`)
    
    if (global.DEV && process.argv.includes('--verbose'))
      args[0] && console.log(args)
  }

  return acc
}, {  })