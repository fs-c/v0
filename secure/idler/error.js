module.exports = (err, req, res, next) => {
  res.render('../idler/views/status', {
    status: 'error',
    message: global.DEV ? err.message : err.message + err.stack,
    root: global.ROOT,
    idler: global.IDLER
  })
}