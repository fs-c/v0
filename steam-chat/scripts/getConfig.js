const fs = require('fs')
const path = require('path')

const ARGS = process.argv.slice(2)
const CONFIG = path.join(require('os').homedir(), '.steam.json')

global.DEV = process.env.NODE_ENV === 'dev' || process.env.NODE_ENV === 'development'

// Get account in the following order:
//  - process args --user & --pass
//  - config file default
//  - --account in config file

const validJSON = path => {
  let failed = false
  try { require(path) } catch(e) { failed = true }
  return !failed
}

if (ARGS.includes('--user') && ARGS.includes('--pass')) {
  return global.ACCOUNT = {
    accountName: ARGS[ARGS.indexOf('--user') + 1],
    password: ARGS[ARGS.indexOf('--pass') + 1]    
  }
}

if (fs.existsSync(CONFIG) && validJSON(CONFIG)) {
  let account
  let accounts = require(CONFIG)
    
  if (ARGS.includes('--account'))
    account = accounts[ARGS[ARGS.indexOf('--account') + 1]]
  else 
    account = accounts.default

  if (account.apikey || accounts.default.apikey)
    global.API_KEY = account.apikey || accounts.default.apikey

  if (account)
    return global.ACCOUNT = account
  else throw new Error(`Couldn't get an account.`)
}