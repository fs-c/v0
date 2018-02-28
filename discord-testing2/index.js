require('dotenv').config();

const debug = require('debug')('app');
const Discord = require('discord.io');

const client = new Discord.Client({
  autorun: true,
  token: process.env.TOKEN,
});

client.on('ready', (event) => {
  debug(event);

  client.on('presence', debug);

  debug(client.users);

  client.getAllUsers((err) => debug(err, client.users));
});

client.on('disconnect', debug);
