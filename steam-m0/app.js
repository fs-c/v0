const PATH = process.env.DATA_PATH || './accountdata'
const ENV  = process.env.NODE_ENV || 'prod'

const ACCOUNT = require(PATH).bot

require('console-stamp')(console, 'HH:MM:ss.l')

const Bot = require('./Bot')
let bot = new Bot(ACCOUNT)

bot.on('loggedOn', (a, b, c) => console.log(`loggedOn: ${a} ${b} ${c}`))
