global.IDLER = global.ROOT + 'idler/'
global.CRYPTO_KEY = process.env.CRYPTO ||
  require(global.PATHS.config).crypto_key

const MONGODB_PASS = process.env.IDLER_DB_PASS ||
  require(global.PATHS.config).idler_mongo_pass
const MONGODB_STRING = `mongodb://root:${MONGODB_PASS}`
  + `@ds159187.mlab.com:59187/fsoc-idler`

const log = require('../logger')

// Set up DB.
const mongoose = require('mongoose')
mongoose.promise = global.Promise

const db = mongoose.connection

global.IDLER_DB = mongoose.createConnection(MONGODB_STRING, {
  useMongoClient: true
})

db.on('error', log.error)
db.once('open', () => log.info('connected to idler db'))

const Account = require('./models/Account')

// Set up idler.
const idler = new (require('./Idler'))()

Account.find({  }, (err, accounts) => {
  if (err)
    return log.error(`failed to get accounts`)

  idler.add(accounts)
})

// Set up router to export.
const router = module.exports = require('express').Router()
const crypto = require('crypto')

router.get('/', (req, res, next) => {
  Account.find({ owner: req.user.username }, (err, accounts) => {
    if (err) return next(err)

    res.render('../idler/views/index', {
      idler: global.IDLER, root: global.ROOT, accounts
    })
  })
})

router.post('/add', (req, res, next) => {
  if (!(req.body.name && req.body.pass))
    return next(new Error('Invalid POST: missing data.'))
  
  let cipher = crypto.createCipher('aes192', global.CRYPTO_KEY)
  let encrypted = cipher.update(req.body.pass, 'utf8', 'hex')
  encrypted +=  cipher.final('hex')

  let data = {
    name: req.body.name,
    pass: encrypted,
    shasec: req.body.shasec,
    owner: req.user.username
  }

  idler.add([ data ])

  new Account(data).save(err => err 
    ? next(err)
    : res.render('../idler/views/status', {
      root: global.ROOT, status: 'success', message: 'Added account.'
    })
  )
})

router.get('/edit/:id', (req, res, next) => {
  res.render('../idler/views/edit', {
    idler: global.IDLER, root: global.ROOT, id: req.params.id
  })
})

router.post('/edit/:id', (req, res, next) => {
  Account.findById(req.params.id, (err, account) => {
    let bdy = req.body

    if (err) return next(err)
    if (!account) return next(new Error('Account not found.'))

    if (req.user.access !== -1 || req.user.username !== account.owner)
      return next(new Error('Not authorized to perform this action.'))

    account.name = bdy.name || account.name
    account.pass = bdy.pass || account.pass
    account.shasec = bdy.shasec || account.shasec
    account.twoFactorCode = bdy.twoFactorCode || account.twoFactorCode

    idler.update([ account ])

    account.save(err => err 
      ? next(err)
      : res.render('../idler/views/status', {
        root: global.ROOT, status: 'success', message: 'Updated account.'
      }))
  })
})

router.get('/delete/:id', (req, res, next) => {
  Account.findById(req.params.id, (err, account) => {
    if (err) return next(err)
    if (!account) return next(new Error('Account not found.'))

    if (req.user.access !== -1 || req.user.username !== account.owner)
      return next(new Error('Not authorized to perform this action.'))

    account.remove(err => err 
      ? next(err)
      : res.render('../idler/views/status', {
        root: global.ROOT, status: 'success', message: 'Deleted account.'
      }))
  })
})
// router.use(require('./error'))