const got = require('got');
const cheerio = require('cheerio');
const log = require('./logger')('steam');
const { URLSearchParams } = require('url');

const getFriends = async (base) => {
    const path = 'http://api.steampowered.com/ISteamUser/GetFriendList/v0001/?';
    const params = new URLSearchParams({
        steamid: base,
        relationship: 'all',
        key: process.env.STEAM_API_KEY,
    });

    log.trace({ from: base }, 'getting friend list')

    const { body } = await got(path + params.toString(), { json: true });

    const friends = body.friendslist.friends;

    log.trace({ friends: friends.length, from: base }, 'got friend list');

    return friends.map((e) => e.steamid);
};

const getCountry = async (id) => {
    const uri = 'https://steamcommunity.com/profiles/' + id;
    const { body } = await got(uri);
    const $ = cheerio.load(body);
    const flag = $('img.profile_flag');

    log.trace({ id }, 'got profile');

    if (flag && flag.attr('src')) {
        const code = flag.attr('src').split('/').reverse()[0].split('.')[0];

        log.trace({ code }, 'parsed flag');

        return code;
    } else {
        log.trace('no country set');

        return 'zz';
    }
};

exports.getFriends = getFriends;
exports.getCountry = getCountry;
