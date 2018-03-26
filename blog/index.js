#!/usr/bin/env node

require('dotenv').config();

const rls = require('readline-sync');
const program = require('commander');
const mongoose = require('mongoose');

// const { Entry } = require('./src/models/Entry');

const init = async (cmd) => {
  const db = {
    uri: program.dbUri || process.env.DB_URI,
    user: program.dbUser || process.env.DB_USER,
    password: program.dbPass || process.env.DB_PASS,
  };
  
  try {
    await mongoose.connect(`mongodb://${db.user}:${db.pass}@${db.uri}`);

    const entries = await Entry.find({});

    if (entries && entries[0]) { /***/ }
  } catch(err) {
    console.error(`error: ${err.message || err}`);
  }
};

const start = (cmd) => {
  const { app } = require('./src/server');

  app.listen(program.port || process.env.PORT || 8080);
};

program.command('init')
  .option('--db-uri <uri>', 'The URI of the MongoDB database.')
  .option('--db-user <name>', 'The name of the DB user.')
  .option('--db-pass <pass>', 'The password of the DB user.')  
  .description('Initialise a new blog.')
  .action((cmd) => init(cmd).catch(console.error));

program.command('start')
  .option('-p, --port <p>', 'The port to start the server on.')
  .description('Start the server.')
  .action(start);

program.version(require('./package.json').version);

program.parse(process.argv);