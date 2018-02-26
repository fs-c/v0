const router = module.exports = require('express').Router()

router.get('/', (req, res) => res.render('../trader/index', { root: global.ROOT }))