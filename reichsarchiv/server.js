const PORT = process.env.PORT || 8083

const express = require('express')
const app = express()

app.set('view engine', 'ejs')

app.enable('trust proxy')

app.use(require('helmet')())        // Basic security.
app.use(require('morgan')('dev', {  // Logging.
  stream: { write: msg => require('../logger').verbose(msg.trim()) }
}))

app.use(express.static('files'))  // Images.
app.use(express.static('static')) // Pages.

app.use('/', require('./fs'))     // Filesystem; Upload/Delete.

app.use(require('./error'))       // Handle errors.

app.listen(PORT)
