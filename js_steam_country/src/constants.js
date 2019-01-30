require('dotenv').config();

const defaults = {
    interval: 60 * 1000,
    logLevel: process.env.NODE_ENV === 'production' ? 'warn' : 'trace',
};

/* We should do this before requiring the logger */
exports.logLevel = process.env.LOG_LEVEL || defaults.logLevel;

const log = require('./logger')('constants');

exports.steam = {};

if (!(exports.steam.apiKey = process.env.STEAM_API_KEY)) {
    log.fatal('`STEAM_API_KEY` env variable is missing');

    process.exit(1);
}

if (!(exports.fallbackID = process.env.FALLBACK_ID)) {
    log.warn('`FALLBACK_ID` is not set, make sure that the id pool is large');
}

exports.interval = process.env.INTERVAL || defaults.interval;
