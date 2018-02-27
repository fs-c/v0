const Client = require('../components/Client')

const get = module.exports = (api, req) => {
  const clients = api.CLIENTS
  const accounts = api.ACCOUNTS

  const name = req.params.account
  const account = accounts[name] || // Name can be either alias or username.
    Object.values(accounts).filter(e => e.accountName === name)[0]

  if (!clients[name] && account)
    global.api.CLIENTS[name] = new Client(account)

  const client = global.api.CLIENTS[name]

  if (!client || !account)
    throw new Error('Insufficient data.')

  return { client, account }
}