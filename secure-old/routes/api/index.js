const api = require('../../api/')
const router = module.exports = require('express').Router()

router.get('/', (req, res) => res.render('api', { api }))

api.map(route => {
  router.get(route.path, (req, res, next) => {
    if (route.level && route.level < req.user.access)
      return next(
        new Error(`Not authorized to run this function `
          + `(${func.level} < ${req.user.access})`)
      )

    try {
      route.function()
        .then((...ret) => res.json({ status: 'success', data: ret }))
        .catch(err => next(err))
    } catch(e) { next(e) }
  })
})