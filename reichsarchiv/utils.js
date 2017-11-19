const MAX_FILES = 100

const log = require('../logger')

const fs = require('fs')

const cleanup = (maxNum = MAX_FILES) => {
  const names = fs.readdirSync('./files')
    .sort((a, b) => parseInt(a.slice(0, a.lastIndexOf('.') + 3), 10) - parseInt(b.slice(0, b.lastIndexOf('.') + 3), 10))

  if (names.length >= maxNum)
    for (const name of names.slice(0, names.length - maxNum)) {
      fs.unlink(`./files/${name}`, err => { if (err) log.error(err) })
      fs.unlink(`./thumbs/${name}`, err => { if (err) log.error(err) })
      log.debug(`deleted file ${name}`)
    }
}

exports.cleanup = cleanup
