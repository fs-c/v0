const MAX_FILES = 100
const FILETYPES = [ 'image/jpeg', 'image/png', 'image/gif' ]

const log = require('../logger')

const fs = require('fs')
const Jimp = require('Jimp')

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

const uploadFile = (req, res) => {
  if (!req.files)
    return res.status(400).send('No files were uploaded.')

  cleanup()

  const file = req.files.file
  const name = Date.now() + file.name.slice(file.name.lastIndexOf('.'))

  if (!FILETYPES.includes(file.mimetype))
    return res.status(415).send('Filetype not allowed.')

  Jimp.read(file.data, (err, image) => {
    if (err) return log.error(err)

    image.resize(Jimp.AUTO, 100)
      .crop(0, 0, 100, 100)
      .write('./thumbs/' + name)
  })

  file.mv('./files/' + name).then(() => {
    res.send('File uploaded. /' + name)
    log.debug(`saved file ${name}`)
  }).catch(err => res.status(500).send(err.message))
}

exports.upload = uploadFile

const deleteFile = (req, res) => {
  const name = req.params.name

  if (req.params.name === 'all')
    cleanup(0)

  if (fs.existsSync(`./files/${name}`))
    fs.unlinkSync(`./files/${name}`)
  if (fs.existsSync(`./thumbs/${name}`))
    fs.unlinkSync(`./files/${name}`)

  return res.send(`Deleted file(s).`)
}

exports.delete = deleteFile

const rawIndex = (req, res) => {
  res.status(200).json(fs.readdirSync('./files'))
}

exports.rawIndex = rawIndex
