const logger = require('winston')
const moment = require('moment')

logger.remove(logger.transports.Console)
logger.add(logger.transports.Console, {
  level: 'silly',
  colorize: true,
  timestamp: function() {
    return moment().format('YYYY-MM-DD hh:mm:SSA')
  }
})

module.exports = logger
