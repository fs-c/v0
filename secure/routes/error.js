const handle = module.exports = (err, req, res, next) => {
  res.status(500).json({
    status: 'error',
    // Split error stack for readability.
    message: global.DEV ? err.stack.split('\n') : err.message
  })
  
  next()
}