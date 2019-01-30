const pino = require('pino');

const { logLevel } = require('./constants');

const create = (name = 'default') => {
    return pino({
        name,
        base: null,
        /* Do this to prevent a crash in case logLevel isn't set */
        level: logLevel || 'trace',
    });
};

module.exports = create;
