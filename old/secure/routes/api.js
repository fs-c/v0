const api = require('../api/')
const router = module.exports = require('express').Router()

router.get('/', (req, res) => {
  let routes = api.filter(route => req.user.access <= route.level)
  res.render('api', {
    routes,
    hidden: api.length - routes.length,
    root: global.ROOT 
  })
})

api.map(route => {
  router.get(route.path, (req, res, next) => {
    // Return and pass an error along if not authed.
    if (route.level && route.level < req.user.access)
      return next(
        new Error(`Not authorized to run this function `
          + `(${route.level} < ${req.user.access})`)
      )

    // Ensure required parameters and queries are present.
    if (route.params) {
      route.params.forEach(param => {
        if (param.required && !req.params[param.name])
          return next(new Error(`Missing required parameter ${param.name}!`))
      })
    }
    if (route.queries) {
      route.queries.forEach(query => {
        if (query.required && !req.query[query.name])
          return next(new Error(`Missing required URL query ${query.name}!`))
      })
    }

    route.function({ params: req.params, query: req.query })
      .then((...ret) => res.json({ status: 'success', data: ret }))
      .catch(err => next(err))
  })
})