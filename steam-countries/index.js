require('dotenv').config();

const got = require('got');
const cheerio = require('cheerio');
const r = require('rethinkdbdash')();
const { URLSearchParams } = require('url');
const log = require('pino')({
    base: null,
    name: 'sct',
    level: 'trace',
});

const fetchIDs = async (base) => {
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
}

const fetchCountry = async (id) => {
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

let tn = 0;
let lastFinished = true;

/**
 * End tick, logging a final status and setting it to finished.
 */
const endTick = (msg, obj = {}) => {
    if (typeof msg === 'string') {
        obj.msg = msg;
    } else { obj = msg; }

    obj.tick = tn;

    log.info(obj);

    lastFinished = true;
}

const tick = async () => {
    tn++;

    log.trace({ tick: tn }, 'starting tick');

    const pool = r.db('steam').table('id_pool');
    const countries = r.db('steam').table('countries');

    if (!lastFinished) {
        return endTick('last unfinished, skipping');
    }

    lastFinished = false;

    const subpool = await pool.filter({ open: true }).limit(1);
    const active = subpool[0];

    log.trace('fetched subpool');

    // No open ID found, refill pool.
    if (!active) {
        // Fetch a random ID to use as base.
        const sample = (await pool.sample(1))[0];
        // If pool is empty use fallback ID.
        const base = sample ? sample.id : process.env.FALLBACK_ID;

        log.debug({ base }, 'rebuilding pool');

        let ids;
        try {
            ids = (await fetchIDs(base))
                .map((id) => ({ id, ts: Date.now(), open: true }));
        } catch (err) {
            // Will retry on next tick.
            return endTick('failed fetching ids', err);
        }

        // The ID is already in the database, leave unchanged.
        const conflict = (id, od, nd) => {
            log.trace('id insertion conflict');
            return od;
        };

        let res;
        try {
            res = await pool.insert(ids, { conflict });
        } catch (err) {
            return endTick('failed inserting ids', err);
        }

        return endTick('inserted new ids', res);
    }

    log.debug(active, 'processing id');

    const country = await fetchCountry(active.id);
    const exists = (await countries.get(country)) !== null;

    let cUpd;
    if (!exists) {
        log.trace('found country entry, incrementing');
        cUpt = await countries.get(country).update({
            occ: r.row('occ').add(1).default(1),
        });
    } else {
        log.trace('no country entry found, inserting');
        cUpt = await countries.insert({ id: country, occ: 1 });
    }

    // Close the id and update the timestamp.
    const pUpd = await pool.get(active.id).update({
        open: false,
        ts: Date.now(),
    });

    log.trace(pUpd, 'closed id in pool');

    return endTick('updated country', cUpd);
}

/**
 * Build the given database structure.
 * 
 * Structure example: { db1: [ table1, table2 ], db2: [ table3 ] }
 */
const setup = async (structure) => {
    for (const db of Object.keys(structure)) {
        log.trace('creating %s', db);

        try { await r.dbCreate(db) } catch (err) {
            log.trace(err.msg || err.message);            
        }

        for (const table of structure[db]) {
            log.trace('creating %s.%s', db, table)

            try { await r.db(db).tableCreate(table) } catch (err) {
                log.trace(err.msg || err.message);
            }
        }
    }
}

(async () => {

await setup({
    steam: [ 'id_pool', 'countries' ],
});

const interval = process.env.INTERVAL || 60 * 1000;
tick().catch((err) => log.warn(err));
setInterval(() => {
    tick().catch((err) => log.warn(err));
}, interval);

})().catch((err) => log.fatal(err));
