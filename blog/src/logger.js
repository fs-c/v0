const { createLogger, transports, format } = require('winston');

const logger = exports.log = createLogger({
  format: format.combine(
    format.splat(),
    format.timestamp(),
    format.simple()
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'log/combined.log' }),
    new transports.File({ filename: 'log/error.log', level: 'error',  })    
  ]
});
