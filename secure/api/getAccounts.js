const getAccounts = module.exports = () => {
  return new Promise((resolve, reject) => {
    resolve(global.api.ACCOUNTS)
  })
}