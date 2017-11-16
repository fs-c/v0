const log = require('../logger')

const fs = require('fs')

const express = require('express')
const app = express()

const MAX_FILES = 100

app.enable('trust proxy')

app.use(require('helmet')())        // Basic security.
app.use(require('morgan')('dev', {  // Logging.
  stream: { write: msg => log.verbose(msg.trim()) }
}))

if (!fs.existsSync('./files'))
  fs.mkdirSync('./files')

app.use(express.static('files'))
app.use(express.static('static'))

const fileUpload = require('express-fileupload')
app.use(fileUpload({ limits: { fileSize: 1024 * 1024 } })) // 1 MiB

const cleanup = (maxNum = MAX_FILES) => {
  const names = fs.readdirSync('./files')
    .map(n => parseInt(n.slice(0, n.lastIndexOf('.jpg')), 10))
    .sort((a, b) => a - b)

  if (names.length >= maxNum)
    for (const name of names.slice(0, names.length - maxNum)) {
      fs.unlink(`./files/${name}.jpg`, err => { if (err) log.error(err) })
      log.debug(`deleted file ${name}.jpg`)
    }
}

app.post('/upload', (req, res) => {
  if (!req.files)
    return res.status(400).send('No files were uploaded.')

  cleanup()

  const file = req.files.file
  const name = Date.now() + file.name.slice(file.name.lastIndexOf('.'))

  file.mv('./files/' + name, err => {
    if (err) return res.status(500).send(err.message)

    res.send('File uploaded. ' + name)
    log.debug(`saved file ${name}`)
  })
})

app.get('/delete/:name', (req, res) => {
  if (req.params.name === 'all') {
    cleanup(0)
    res.send(`Deleted files.`)
  }

  if (fs.existsSync(`./files/${name}`))
    fs.unlinkSync(`./files/${name}`)
})

app.listen(process.env.PORT || 8083)
