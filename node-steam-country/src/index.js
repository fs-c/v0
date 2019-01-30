const { interval, fallbackID } = require('./constants');

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

    /* Meta information is rarely provided, it'd be annoying to log an empty
       meta object almost every tick */
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
        /* This should only fail in very rare cases, it's worth logging the full
           object here */
        log.trace(err);

        /* Set tick to finished even if this failed because the pool will need
           to be refilled in the next tick anyways */
        return finishTick('failed refilling pool');
    }
};

const processID = async (active) => {
    log.debug(active, 'processing id');

    const country = (await getCountry(active.id)) || 'no_country';
    const entry = await countries.get(country);

    let cUpd;
    if (entry !== null) {
        log.trace({ entry }, 'found country entry, incrementing');
        cUpd = await countries.get(country).update({
            occ: r.row('occ').add(1).default(1),
        });
    } else {
        log.trace({ country }, 'no country entry found, inserting');
        cUpd = await countries.insert({ id: country, occ: 1 });
    }

    // Close the id and update the timestamp.
    const pUpd = await pool.get(active.id).update({
        open: false,
        ts: Date.now(),
    });

    log.trace('closed id in pool');

    return finishTick('updated country', cUpd);
};

const runTick = async () => {
    if (!currentTick.finished) {
        endTick('current tick unfinished, skipping');
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
        log.warn(err);
    };

    runTick().catch(tickError);

    setInterval(() => {
        runTick().catch((err) => log.warn(err));
    }, interval);
};

runTicks();
