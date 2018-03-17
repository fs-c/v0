global.api = {  }

global.api.CLIENTS = {  }
global.api.ACCOUNTS = require(require('path').join(
  require('os').homedir(), '.steam.json')
)

const routes = [
  {
    path: '/accounts',
    name: 'getAccounts',
    description: 'Returns all accounts.',
    level: -1
  },
  {
    path: '/codes',
    name: 'getCodes',
    description: 'Returns steam mobile auth codes for all accounts with a shasec property.',
    level: 1
  },
  {
    path: '/:account/getOffers',
    name: 'account/getOffers',
    description: 'Returns outstanding offers for the specified account.',
    params: [
      {
        name: 'account',
        description: 'can be either username or alias',
        required: true
      }
    ]
  },
  {
    path: '/:account/acceptOffer',
    name: 'account/acceptOffer',
    description: 'Accepts the given offer, returns a status.',
    level: 0,
    params: [
      {
        name: 'account',
        description: 'can be either username or alias',
        required: true
      }
    ],
    queries: [
      {
        name: 'id',
        description: 'a valid steam offer id',
        required: true
      }
    ]
  },
  {
    path: '/:account/declineOffer',
    name: 'account/declineOffer',
    description: 'Declines the given offer, returns a status.',
    level: 0,
    params: [
      {
        name: 'account',
        description: 'can be either username or alias',
        required: true
      }
    ],
    queries: [
      {
        name: 'id',
        description: 'a valid steam offer id',
        required: true
      }
    ]
  }
]

const api = module.exports = routes.map(route => {
  try {
    route.level = !route.level ? route.level === 0 ? 0 : 2 : route.level
    route.function = require('./' + route.name)
  } catch(err) {
    // If no module with that name is found, attach stub function.
    route.function = () => { return new Promise((r, reject) => reject(err)) }
  }

  return route
})