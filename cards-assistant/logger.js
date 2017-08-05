const logger = require('winston')
logger.remove(logger.transports.Console)
logger.add(logger.transports.Console, { level: 'silly', colorize: true, timestamp: true })

module.exports = logger

// error: 0,    OK yeah, something broke for real.
// warn: 1,     Something went wrong, it's not t h a t bad though.
// info: 2,     Stuff that just happened and is important for the whole app.
// verbose: 3,  Announcing stuff that is important and about to happen.
//              Stuff that just happened but isn't really that important or special.
// debug: 4,
// silly: 5
