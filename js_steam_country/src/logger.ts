import * as pino from 'pino';

export const create = (name = 'default') => {
    return pino({
        name,
        base: null,
        level: 'trace',
    });
};
