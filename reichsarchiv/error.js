const handle = (err, req, res, next) => {
  res.status(500).render('error', err)
  require('../logger').error(err.message)

  next()
}

module.exports = handle
