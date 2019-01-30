const log = require('./logger')('structure');

const defaultStructure = {
    steam: [ 'id_pool', 'countries' ],    
};

const buildStructure = async (r, struct) => {
    for (const db of Object.keys(struct)) {
        log.trace('creating %s', db);

        try { await r.dbCreate(db).run(); } catch (err) {
            log.trace('database creation error, ignoring');
        }

        for (const table of struct[db]) {
            log.trace('creating %s.%s', db, table);

            try { await r.db(db).tableCreate(table).run(); } catch (err) {
                log.trace('table creation error, ignoring');
            }
        }

        /* TODO: Implement database structure verification */
        const intact = true;
        if (intact) {
            log.info('database %o is intact', db);
        }
    }
};

exports.structure = defaultStructure;
exports.buildStructure = buildStructure;
