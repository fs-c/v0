const { defaults, interval, fallbackID } = require('./constants');

const log = require('./logger')('index');
const r = require('rethinkdbdash')({ silent: true });

const { getFriends, getCountry } = require('./steam');
const { structure, buildStructure } = require('./structure');

let currentTick = {
    number: 0,
    finished: true,
};

(async () => {
    await buildStructure(r, structure);
})();

const pool = r.db('steam').table('id_pool');
const countries = r.db('steam').table('countries');

const endTick = (msg = `concluding tick ${currentTick.number}`, meta = false) => {
    const state = currentTick;

    state.message = msg;

    if (meta) {
        state.meta = meta;
    }

    log.trace(state);
};

const finishTick = (msg, meta) => {
    currentTick.finished = true;

    endTick(msg, meta);
};

const refillPool = async () => {
    /* Fetch a random ID to use as base */
    const sample = (await pool.sample(1))[0];
    /* If pool is empty use fallback ID */
    const base = sample ? sample.id : fallbackID;

    log.debug({ base }, 'rebuilding pool');

    const conflict = (id, oldID, newID) => {
        log.trace('insertion conflict, preferring original (%o vs %o)', oldID, newID);

        return oldID;
    };

    try {
        const ids = (await getFriends(base)).map((id) => (
            { id, ts: Date.now(), open: true }
        ));

        const res = await pool.insert(Object.keys(ids).map((key) => ids[key]),
            { conflict });

        return finishTick('inserted new ids', res);        
    } catch (err) {
        log.trace(err);

        /* Set tick to finished even if this failed because the pool will need
           to be refilled in the next tick anyways */
        return finishTick('failed refilling pool');
    }
};

const processID = async (active) => {
    log.debug(active, 'processing id');

    const country = (await getCountry(active.id)) || defaults.country;
    const entry = await countries.get(country);

    const result = entry !== null ? (
        await countries.get(country).update({
            occ: r.row('occ').add(1).default(1),
        })
    ) : (
        await countries.insert({ id: country, occ: 1 })
    );

    await pool.get(active.id).update({
        open: false,
        ts: Date.now(),
    });

    log.trace('closed id in pool');

    return finishTick('updated country', result);
};

const runTick = async () => {
    if (!currentTick.finished) {
        return endTick('current tick unfinished, skipping');
    }

    currentTick.number++;
    currentTick.finished = false;

    log.debug(currentTick, 'starting tick');

    const subpool = await pool.filter({ open: true }).limit(1);
    const active = subpool[0];

    log.trace({ active }, 'fetched subpool');

    if (!active) {
        log.trace('no active id, devoting tick to refill');

        return await refillPool();
    }

    return await processID(active);    
};

const runTicks = () => {
    const tickError = (err) => {
        /* All the error recovery and handling logic is elsewhere, just log it
           as warning so prod knows something's going wrong */
        log.warn(err);
    };

    runTick().catch(tickError);

    setInterval(() => {
        runTick().catch(tickError);
    }, interval);
};

runTicks();
