const getOffers = module.exports = req => {
  const { client, account } = require('./getData')(global.api, req)
  
  return new Promise((resolve, reject) => {
    if (!account || !account.shasec)
      return reject(new Error('Account not found or invalid.'))

    client.getOffers(req.query.state || 'ActiveOnly')
      .then(resolve).catch(reject)
  })
}