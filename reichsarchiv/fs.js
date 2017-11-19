const MAX_FILES = 100
const MIMETYPES = [ 'image/jpeg', 'image/png' ]

const log = require('../logger')
const cleanup = require('./utils').cleanup

const fs = require('fs')
const Jimp = require('Jimp')

const router = require('express').Router()

module.exports = router

const multer = require('multer')
const upload = multer({
  storage: multer.diskStorage({
    destination: './files/',
    filename: (req, file, cb) =>
      cb(null, Date.now() + '.' + file.mimetype.slice(file.mimetype.lastIndexOf('/') + 1))
  }),
  limits: {
    fileSize: 1024 * 1024,
    files: 1
  },
  fileFilter: (req, file, cb) =>
    cb(null, MIMETYPES.includes(file.mimetype))
})

router.post('/upload', upload.single('file'), (req, res) => {
  const file = req.file

  log.debug(`file ${file.filename} uploaded.`)

  res.status(200).send(`Uploaded file. /${file.filename}`)

  Jimp.read(file.path).then(img => {
    img.resize(Jimp.AUTO, 100)
      .crop(0, 0, 100, 100)
      .write('./thumbs/' + file.filename)
  }).catch(log.error) // User doesn't need to know/care about this.
})

router.get('/delete', (req, res) => {
  const name = req.params.name

  if (req.params.name === 'all')
    return cleanup(0)

  if (fs.existsSync(`./files/${name}`))
    fs.unlinkSync(`./files/${name}`)
  if (fs.existsSync(`./thumbs/${name}`))
    fs.unlinkSync(`./thumbs/${name}`)

  return res.send(`Deleted file(s).`)
})
