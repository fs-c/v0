const r = require('rethinkdbdash')();

const setup = async () => {
  const structure = {
    steam: [ 'id_pool' ],
  };

  for (const db of Object.keys(structure)) {
    try { await r.dbCreate(db); } catch (err) {
      console.error(err.msg);
    }

    for (const table of structure[db]) {
      try { await r.db(db).tableCreate(table); } catch (err) {
        console.error(err.msg);
      }
    }
  }
}

(async () => {

await setup();

const { worker } = require('./worker');
worker(r);

})().catch(console.error);