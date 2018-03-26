const rdb = require('rethinkdb');

rdb.connect({ host: 'localhost', port: 28015 }, (err, conn) => {
  if (err) { return console.error(err.message); }

  console.log('connected');
});