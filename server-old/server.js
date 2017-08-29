const log = require('./logger')

const cPATH = process.env.COMMENTS_PATH || './comments/comments.json'
const sPATH = process.env.SLIM_PATH || './comments/slim.json'

if (process.env.CLEAR === 'true') {
  const fs = require('fs')
  fs.writeFileSync(cPATH, JSON.stringify([]))
  fs.writeFileSync(sPATH, JSON.stringify({ start: Date.now() }))

  log.info(`cleared old files.`)
}


let app = require('./app')
const PORT = 8080

app.listen(PORT, () => log.info(`server listening on ${PORT}.`))
