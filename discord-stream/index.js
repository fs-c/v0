#!/usr/bin/env node

const moment = require('moment');
const Discord = require('../discord-client/');
const args = require('minimist')(process.argv.slice(2));

const configPath = args.config 
  || require('path').join(require('os').homedir(), '.discord.json');

const config = require('fs').existsSync(configPath) 
  ? require(configPath) : undefined;

const token = args.token || process.env.token || config.token;

if (!token) {
  throw new Error('No token provided.');
}

const client = new Discord.Client(token);

client.connect();

// Logs a parsed discord message object to the console.
const log = (message) => {
  console.log(
    `${moment(message.timestamp).format('HH:mm:ss')} `
    + `(${Date.now() - message.timestamp}) - `
    + `${message.author.webhook_id || message.author.username}: `
    + `${message.content}`
  );
}

client.on('message', log);
