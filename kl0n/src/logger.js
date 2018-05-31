const IN_DEV = process.env.NODE_ENV !== 'production';

const pino = require('pino');

const create = (name) => {
    return pino({
        name,
        base: null,
        level: IN_DEV ? 'trace' : 'info',
    });
}

module.exports = create;