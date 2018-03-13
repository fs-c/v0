#!/usr/bin/env node

const moment = require('moment');
const Discord = require('../discord-client/Client');
const args = require('minimist')(process.argv.slice(2));

const { join } = require('path');
const { homedir } = require('os');
const { existsSync } = require('fs');

let token;

// TODO: Ugly.
if (args.token) {
  token = args.token;
} else if (args.config) {
  try {
    token = require(args.config).token;
  } catch (e) { throw e }
} else if (existsSync(join(homedir(), '.discord.json'))) {
  try {
    token = require(join(homedir(), '.discord.json')).token;
  } catch(e) { throw e }
} else if (process.env.token) {
  token = process.env.token;
}

if (!token) {
  throw new Error('No token provided.');
}

const client = new Discord.Client(token);

const log = (message) => {
  console.log(
    `${moment(message.timestamp).format('HH:mm:ss')} `
    + `(${Date.now() - message.timestamp}) - `
    + `${message.author.webhook_id || message.author.username}: `
    + `${message.content}`
  );
};

client.connect()
  .then(() => console.log('connected'))
  .catch(console.error)

client.on('message', log);
