import { get } from 'got';
import { load } from 'cheerio';
import * as assert from 'assert';
import { create } from './logger';

const log = create('steam');

const API_KEY = process.env.STEAM_API_KEY;
assert(API_KEY, 'STEAM_API_KEY env value is required');

interface IFriend {
    steamid: string;
    friend_since: number;
    relationship: 'friend';
}

/**
 * Get the friend list for a given Steam ID using the Steam Web API.
 *
 * @param base - Steam64ID
 */
export const getFriends = async (base: number) => {
    const path = 'http://api.steampowered.com/ISteamUser/GetFriendList/v0001';
    const { body } = await get(path, {
        json: true,
        query: {
            key: API_KEY,
            relationship: 'all',
            steamid: base.toString(),
        },
    });

    const friends: IFriend[] = (body.friendslist || {}).friends || [];
    log.trace({ base, friends: friends.length }, 'got friend list');

    return friends.map((e) => parseInt(e.steamid, 10));
};

/**
 * Get the country code a Steam ID is associated with.
 *
 * @param id - Steam64ID
 */
export const getCountry = async (id: number) => {
    const { body } = await get('https://steamcommunity.com/profiles/' + id);

    log.trace({ id }, 'got profile');

    const $ = load(body);
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
