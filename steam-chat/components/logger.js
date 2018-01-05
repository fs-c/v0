const winston = require('winston')
const moment = require('moment')

const config = {
  levels: {
    error: 0,
    warn: 1,
    chat: 2,
    event: 3,
    verbose: 4,
  },
  colors: {
    error: 'red',
    warn: 'yellow',
    chat: 'white',
    event: 'grey',
    verbose: 'cyan'
  }
}

const logger = module.exports = new winston.Logger({
  levels: config.levels,
  transports: [
    new winston.transports.Console({
      level: 'silly',
      handleExceptions: false,
      json: false,
      colorize: true,
      timestamp: () => moment().format('YYYY-MM-DD hh:mm:SSA')
    })
  ]
})