/*

Find MD5 hashes which, in hexadecimal, start with at least five zeroes.

*/

const md5 = require('md5')

const START = 5000000
const VALID = '000000'

const bruteforce = (key) => {
  let i = 5000000

  while (true) {
    let hash = md5(key + ++i)
    let valid = hash.slice(0, 5) == VALID

    console.log(`hash ${hash.slice(0, 5)} is ${!valid ? 'in' : ''}valid (${i})`)

    if (valid) return i
  }
}

console.log('lowest valid i: ', bruteforce('yzbqklnj'))