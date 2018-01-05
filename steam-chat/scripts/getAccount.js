const fs = require('fs')
const totp = require('steam-totp')
const rls = require('readline-sync')

const choose = module.exports = () => {
  if (fs.existsSync('../../steamdata.json') && !rls.keyInYN('Input Manually?')) {
    let accounts = require('../../../steamdata.json')
    let account = Object.values(accounts)[
      rls.keyInSelect(Object.keys(accounts), 'Choose account: ')
    ]

    account.twoFactorCode = account.shasec
      ? totp.getAuthCode(account.shasec)
      : rls.question('Code: ')

    return account
  } else {
    return {
      accountName: rls.question('Name: '),
      password: rls.question('Pass: ', { hideEchoBack: true }),
      twoFactorCode: rls.question('Code: ') || undefined
    }
  }
}