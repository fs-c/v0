const handle = module.exports = (err, req, res, next) => {
  res.status(500).json({
    status: 'error',
    message: err.message
  })
  
  next()
}