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
  const { server } = require('./src/server');
};

program.command('init')
  .description('Initialise a new blog.')
  .action((cmd) => init(cmd).catch(console.error));

program.command('start')
  .description('Start the server.')
  .action(start);

program.parse(process.argv);