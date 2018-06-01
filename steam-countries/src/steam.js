const got = require('got');
const cheerio = require('cheerio');
const log = require('./logger')('steam');

const API_KEY = process.env.STEAM_API_KEY;
require('assert')(API_KEY);

/**
 * Get the friend list for a given Steam ID using the Steam Web API.
 * 
 * @param {number} base - Steam64ID
 */
const getFriends = async (base) => {
    const path = 'http://api.steampowered.com/ISteamUser/GetFriendList/v0001';
    const { body } = await got(path, {
        json: true,
        query: {
            key: API_KEY,
            relationship: 'all',
            steamid: base.toString(),
        },
    });

    const friends = (body.friendslist || {}).friends || [];
    log.trace({ base, friends: friends.length }, 'got friend list');

    return friends.map((e) => e.steamid);
};

/**
 * Get the country code a Steam ID is associated with.
 * 
 * @param {number} id - Steam64ID
 */
const getCountry = async (id) => {
    const { body } = await got('https://steamcommunity.com/profiles/' + id);

    log.trace({ id }, 'got profile');    

    const $ = cheerio.load(body);
    const flag = $('img.profile_flag');

    if (flag && flag.attr('src')) {
        const code = flag.attr('src').split('/').reverse()[0].split('.')[0];

        log.trace({ code }, 'parsed flag');

        return code;
    } else {
        log.trace('no country set');

        return null;
    }
};

exports.getFriends = getFriends;
exports.getCountry = getCountry;
