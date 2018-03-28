const { createLogger, transports, format } = require('winston');

const custom = format.printf((info) => {
  return `${info.timestamp} - [${info.level}]: ${info.message}`;
});

const logger = exports.log = createLogger({
  level: 'silly',
  format: format.combine(
    format.splat(),
    format.timestamp(),
    custom
  ),
  transports: [
    new transports.Console({ level: 'silly' }),
    new transports.File({ filename: 'log/combined.log' }),
    new transports.File({ filename: 'log/error.log', level: 'error',  })    
  ]
});
