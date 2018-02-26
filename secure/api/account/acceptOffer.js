const acceptOffer = module.exports = req => {
  return new Promise((resolve, reject) => {
    const { client, account } = require('./getData')(global.api, req)
    const id = req.query.id

    if (!id) return reject(new Error('No offer id provided!'))

    client.manager.getOffer(id, (err, offer) => {
      if (err) return reject(err)

      offer.accept((err, status) => {
        if (err) return reject(err)

        resolve(status)
      })
    })
  })
}