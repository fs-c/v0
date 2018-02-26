const getAccounts = module.exports = () => {
  return new Promise((resolve, reject) => {
    resolve(require(
      require('path').join(require('os').homedir(), '.steam.json')
    ))
  })
}