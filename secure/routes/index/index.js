const router = module.exports = require('express').Router()

router.get('/', (req, res) => { res.render('index', { user: req.user }) })