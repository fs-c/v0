const api = require('../../api/')
const router = module.exports = require('express').Router()

const functions = [
  {
    route: '/accounts',
    name: 'getAccounts',
    description: 'returns all accounts in .steam.json',
    level: -1
  },
  {
    route: '/codes',
    name: 'getCodes',
    description: 'returns steam auth codes for all accounts in .steam.json'
  }
]

router.get('/', (req, res) => res.render('api', { functions }))

functions.map(func => {
  router.get(func.route, (req, res, next) => {
    if (func.level && func.level < req.user.access)
      return next(
        new Error(`Not authorized to run this function `
          + `(${func.level} < ${req.user.access})`)
      )

    try {
      api[func.name]()
        .then((...ret) => res.json({ status: 'success', data: ret }))
        .catch(err => next(err))
    } catch(e) { next(e) }
  })
})