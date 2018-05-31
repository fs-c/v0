require('dotenv').config();

const r = require('rethinkdbdash')();
const log = require('pino')({
    base: null,
    name: 'index',
    level: 'trace',
});

const setup = async (structure) => {
    for (const db of Object.keys(structure)) {    
        await trca(r.dbCreate, db);

        for (const table of structure[db]) {
            await trca(r.db(db).tableCreate, table);
        }
    }

    const trca = async (func, ...params) => {
        try { await func(...params) } catch (err) {
            console.error(err.msg);            
        }
    }
}

let tn = 0;
let lastFinished = true;
const tick = async () => {
    tn++;

    const pool = r.db('steam').table('id_pool');
    const countries = r.db('steam').table('countries');

    if (!lastFinished) {
        return endTick('last unfinished, skipping');
    }

    lastFinished = false;

    const active = await pool.filter({ open: true }).limit(1);

    if (!active) {
        log.debug('no open id in pool');

        const ids = (await fetchIDs(process.env.FALLBACK_ID)).map((id) => ({
            id,
            ts: 0,
            open: false,
        }));

        const res = await pool.insert(ids, {
            conflict: (id, od, nd) => {
                log.trace({ id }, 'id insertion conflict');
                return od;
            },
        });

        return endTick('inserted new ids', res);
    }

    log.debug(active, 'processing id');

    const country = await fetchCountry(active.id);

    const res = await countries.insert({ id: country, occ: 1 }, {
        conflict: (id, od, nd) => {
            log.trace('country insertion conflict');

            od.occ++;
            return od;
        },
    });

    return endTick('updated country', res);
}

const endTick = (msg, obj = {}) => {
    if (typeof msg === 'string') {
        obj.msg = msg;
    } else { obj = msg; }

    obj.tick = tn;

    log.info(obj);

    lastFinished = true;
}

setInterval(tick, 1000);
