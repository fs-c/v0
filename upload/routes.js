const MAX_FILES = 100

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

const upload = (req, res) => {
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
}

exports.upload = delete

const delete = (req, res) => {
  if (req.params.name === 'all') {
    cleanup(0)
    res.send(`Deleted files.`)
  }

  if (fs.existsSync(`./files/${name}`))
    fs.unlinkSync(`./files/${name}`)
}

exports.delete = delete
