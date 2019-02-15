require('dotenv').config();

const defaults = {
    /* Tick interval */
    interval: 60 * 1000,
    /* Fallback country code */
    country: 'no_country',
    logLevel: process.env.NODE_ENV === 'production' ? 'warn' : 'trace',
};

/* We should do this before requiring the logger */
exports.logLevel = process.env.LOG_LEVEL || defaults.logLevel;

const log = require('./logger')('constants');

const missing = (name, code = 1) => {
    log.fatal(`'${name}' env variable is missing but required`);

    process.exit(code);
};

exports.steam = {};

if (!(exports.steam.apiKey = process.env.STEAM_API_KEY)) {
    missing('STEAM_API_KEY');
}

if (!(exports.fallbackID = process.env.FALLBACK_ID)) {
    log.warn('`FALLBACK_ID` is not set, make sure that the id pool is large');
}

exports.interval = process.env.INTERVAL || defaults.interval;
