const handle = (err, req, res, next) => {
  require('../logger').warn(err.message)
  require('../logger').debug(err)

  res.json({
    success: false,
    error: global.DEV ? err : undefined
  })

  next()
}

module.exports = handle
