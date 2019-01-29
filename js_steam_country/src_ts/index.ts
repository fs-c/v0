import { config } from 'dotenv';
config();

import { r, RDatum } from 'rethinkdb-ts';
import { create } from './logger';
import { getFriends, getCountry } from './steam';

interface IPoolItem {
    id: number;
    ts: number;
    open: boolean;
}

const log = create('index');

let tn = 0;
let lastFinished = true;

/**
 * - Make tables globals.
 * - Add some performance measuring.
 * - Centralise logging.
 * - Deal with Steam ratelimiting.
 *      * Appropiately slow down/cancel queries.
 *      * Implement proxying (aka get some IPs to use).
 */

 /**
  * TODO: This is a stupid implementation.
  */
const endTick = (msg: any, obj: any = {}) => {
    if (typeof msg === 'string') {
        obj.msg = msg;
    } else { obj = msg; }

    log.debug(obj);
};

 /**
  * TODO: This is a stupid implementation.
  */
const finishTick = (msg: any, optObj?: any) => {
    endTick(msg, optObj || {});

    lastFinished = true;
};

/**
 * Fetch fresh IDs and insert them to the pool.
 */
const refillPool = async () => {
    const pool = r.db('steam').table('id_pool');

    // Fetch a random ID to use as base.
    const sample = (await pool.sample(1).run())[0];
    // If pool is empty use fallback ID.
    const base = sample ? sample.id : process.env.FALLBACK_ID;

    log.debug({ base }, 'rebuilding pool');

    let ids;
    try {
        ids = (await getFriends(base))
            .map((id) => ({ id, ts: Date.now(), open: true }));
    } catch (err) {
        // Will retry on next tick.
        return finishTick('failed fetching ids', err);
    }

    const conflict = (id: RDatum<any>, oid: RDatum<any>, nid: RDatum<any>) => {
        log.trace({ oid, nid }, 'id insertion conflict, preferring original');

        return oid;
    };

    let res;
    try {
        res = await pool.insert(ids, { conflict }).run();
    } catch (err) {
        return finishTick('failed inserting ids', err);
    }

    return finishTick('inserted new ids', res);
};

/**
 * Process a given ID object, fetching the country, and closing it on success.
 *
 * @param active - An id_pool entry.
 */
const processID = async (active: IPoolItem) => {
    log.debug(active, 'processing id');

    const pool = r.db('steam').table('id_pool');
    const countries = r.db('steam').table('countries');

    const country = (await getCountry(active.id)) || 'zz';
    const entry = await countries.get(country).run();

    let cUpd;
    if (entry !== null) {
        log.trace({ entry }, 'found country entry, incrementing');
        cUpd = await countries.get(country).update({
            occ: r.row('occ').add(1).default(1),
        }).run();
    } else {
        log.trace({ country }, 'no country entry found, inserting');
        cUpd = await countries.insert({ id: country, occ: 1 }).run();
    }

    // Close the id and update the timestamp.
    const pUpd = await pool.get(active.id).update({
        open: false,
        ts: Date.now(),
    }).run();

    log.trace(pUpd, 'closed id in pool');

    return finishTick('updated country', cUpd);
};

/**
 * To be executed at a set interval, the main work loop.
 */
const tick = async () => {
    tn++;

    const pool = r.db('steam').table('id_pool');

    log.trace({ tick: tn }, 'starting tick');

    // Let the previous tick finish.
    if (!lastFinished) {
        return endTick('last unfinished, skipping');
    }

    lastFinished = false;

    const subpool = await pool.filter({ open: true }).limit(1).run();
    const active = subpool[0];

    log.trace({ active }, 'fetched subpool');

    // No open ID found, refill pool.
    if (!active) {
        log.trace('no active id, devoting tick to refill');

        return await refillPool();
    }

    return await processID(active);
};

/**
 * Build the given database structure.
 *
 * Structure example: { db1: [ table1, table2 ], db2: [ table3 ] }
 */
const setup = async (structure: { [key: string]: string[] }) => {
    for (const db of Object.keys(structure)) {
        log.trace('trying to create database "%s"', db);

        try { await r.dbCreate(db).run(); } catch (err) {
            log.trace(err, 'database creation error');

            // Failed to create database, skip its contents.
            continue;
        }

        for (const table of structure[db]) {
            log.trace('trying to create table "%s.%s"', db, table);

            try { await r.db(db).tableCreate(table).run(); } catch (err) {
                log.trace(err, 'table creation error');
            }
        }
    }
};

(async () => {
    await r.connectPool();

    log.trace('initialized connection pool');

    await setup({
        steam: [ 'id_pool', 'countries' ],
    });

    log.trace('database structure intact');

    const interval = process.env.INTERVAL || 60 * 1000;
    tick().catch((err) => log.warn(err));
    setInterval(() => {
        tick().catch((err) => log.warn(err));
    }, interval);

})().catch((err) => log.fatal(err));
