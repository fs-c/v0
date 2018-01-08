const colors = require('colors')
const time = () => require('moment')().format('hh:mm:SSA')

const config = {
  error: 'red',
  debug: 'blue' 
}

colors.setTheme()

const log = module.exports = Object.keys(config).reduce((acc, val, i, arr) => {
  acc[val] = (msg, ...args) => {
    if (val === 'debug' && process.env.NODE_ENV !== 'dev') return
    
    if (typeof msg === 'object') {
      console.log(`${time()}` + `${val} - object`[val])
      console.log(msg)

      return
    }

    console.log(`${time()} ${val[val]} - ${msg}`)
    
    if (process.env.NODE_ENV === 'dev')
      args[0] && console.log(args)
  }

  return acc
}, {  })