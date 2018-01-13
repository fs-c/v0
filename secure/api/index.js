const routes = [
  {
    path: '/accounts',
    name: 'getAccounts',
    description: 'returns all accounts',
    level: -1
  },
  {
    path: '/codes',
    name: 'getCodes',
    description: 'returns steam auth codes for all accounts'
  }
]

module.exports = routes.map(e => {
  e.function = require('./' + e.name)
  return e
})