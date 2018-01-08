const HOME = require('os').homedir
const ARGS = process.argv.slice(2)

const fs = require('fs')
const path = require('path')

const get = module.exports = () => {
  if (fs.existsSync(path.join(homedir, '.steam.json'))) {
    let accounts = require(path.join(homedir, '.steam.json'))
    
    let account = 
      accounts[ARGS[ARGS.indexOf('--account') + 1]] ||
      accounts.default ||
      (ARGS.includes('--user') && ARGS.includes('--pass')) ? {
        accountName: ARGS[ARGS.indexOf('--user') + 1],
        password: ARGS[ARGS.indexOf('--pass') + 1]
      } : undefined

    return account
  }
}