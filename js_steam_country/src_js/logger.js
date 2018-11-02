const pino = require('pino');

const create = (name = 'default') => {
    return pino({
        name,
        base: null,
        level: 'trace',
    });
};

module.exports = create;
