const fs = require('fs');
const path = require('path');
const moment = require('moment');
const { log } = require('@sturmwalzer/logger');

const Manager = require('steam-tradeoffer-manager');

const defaultConfigPath = path.join(require('os').homedir(), '.strd.json');

const verbose = process.env.verbose == 1 ? log : () => {};

const limitArray = (arr) => {
    if (arr.length <= 5) {
        return arr;
    }

    return arr.splice(5, arr.length - 6, '...');
}

const readConfig = (customPath) => {
    const actualPath = customPath ? (
        path.isAbsolute(customPath) ? customPath : path.resolve(customPath)
    ) : defaultConfigPath;

    verbose('actual config path is %o', actualPath);

    if (!fs.existsSync(actualPath) || !fs.statSync(actualPath).isFile()) {
        throw new Error('Config file not found');
    }

    const config = JSON.parse(fs.readFileSync(actualPath));

    if (!config) {
        throw new Error('Config file empty');
    }

    return config;
};

const printTradeOffer = (offer, user) => {
    const partnerId = offer.partner.getSteamID64();

    log(`received trade offer #%o\n`
        + `               by: %o (%o)\n`
        + `               state: %o\n`
        + `               items to receive: %o\n`
        + `               items to give: %o\n`
        + `               this offer was created/last updated %o and will expire %o`
        , offer.id, user ? user.name : partnerId,
        `https://steamcommunity.com/profiles/${partnerId}`,
        Manager.ETradeOfferState[offer.state],
        limitArray(offer.itemsToReceive.map((e) => e.name)),
        limitArray(offer.itemsToGive.map((e) => e.name)),
        moment(offer.updated).toNow(), moment(offer.expires).toNow(),
    );
}

const printChangedOffer = (offer, oldState) => {
    log('offer #%o changed %o -> %o', offer.id, TradeManager.ETradeOfferState[oldState],
        Manager.ETradeOfferState[offer.state]);
}

module.exports = {
    log, verbose,
    readConfig,
    printTradeOffer,
    printChangedOffer,
};