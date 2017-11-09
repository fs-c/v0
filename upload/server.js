const express = require('express')
const app = express()

app.enable('trust proxy')

app.use(require('helmet')()) // Basic security.
app.use(require('morgan')('dev')) // Logging.

app.use(express.static('static'))

const fileUpload = require('express-fileupload')
app.use(fileUpload({ limits: { fileSize: 1024 * 1024 } })) // 1 MiB

app.post('/upload', (req, res) => {
  if (!req.files)
    return res.status(400).send('No files were uploaded.')

  let file = req.files.file
  let name = Date.now() + file.name.slice(file.name.lastIndexOf('.'))

  file.mv('./files/' + name, err => {
    if (err) return res.status(500).send(err)

    res.send('File uploaded. ' + name)
  })
})

app.listen(8083)
