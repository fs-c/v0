#!/usr/bin/env node

const moment = require('moment');
const Discord = require('../discord-client/Client');
const args = require('minimist')(process.argv.slice(2));

const client = new Discord();

const log = (message) => {
  console.log(
    `${moment(message.timestamp).format('HH:mm:ss')} `
    + `(${Date.now() - message.timestamp}) - `
    + `${message.author.webhook_id || message.author.username}`
    + `${message.content}`
  );
};

client.on('message', log);
