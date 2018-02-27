const accounts = global.api.ACCOUNTS

const getCode = module.exports = () => {
  let codes = {  }

  return new Promise((resolve, reject) => {
    for (const account in accounts) {
      let data = accounts[account]
      if (data && data.shasec)
        codes[account] = require('steam-totp').getAuthCode(data.shasec)
    }

    resolve(codes)
  })
}