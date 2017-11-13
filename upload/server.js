const fs = require('fs')

const express = require('express')
const app = express()

app.enable('trust proxy')

app.use(require('helmet')()) // Basic security.
app.use(require('morgan')('dev')) // Logging.

app.use(express.static('files'))
app.use(express.static('static'))

const fileUpload = require('express-fileupload')
app.use(fileUpload({ limits: { fileSize: 1024 * 1024 } })) // 1 MiB

const cleanup = (maxNum = 2) => {
  const names = fs.readdirSync('./files')
    .map(n => parseInt(n.slice(0, n.lastIndexOf('.jpg')), 10))
    .sort((a, b) => a - b)

  if (names.length >= maxNum)
    for (const name of names.slice(0, names.length - maxNum))
      fs.unlinkSync(`./files/${name}.jpg`)
}

app.post('/upload', (req, res) => {
  if (!req.files)
    return res.status(400).send('No files were uploaded.')

  cleanup()

  const file = req.files.file
  const name = Date.now() + file.name.slice(file.name.lastIndexOf('.'))

  file.mv('./files/' + name, err => {
    if (err) return res.status(500).send(err)

    res.send('File uploaded. ' + name)
  })
})

app.listen(process.env.PORT || 8083)
