const moment = require('moment')
const winston = require('winston')

const logger = new winston.Logger({
  transports: [
    new winston.transports.Console({
      level: 'silly',
      handleExceptions: false,
      json: false,
      colorize: true,
      timestamp: () => moment().format('YYYY-MM-DD hh:mm:SSA')
    })
  ],
  exitOnError: false
})

module.exports = logger
