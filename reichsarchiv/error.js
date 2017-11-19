const handle = (err, req, res, next) => {
  res.status(500).render('error', { message: err.message })

  next()
}

module.exports = handle
