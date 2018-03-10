require('dotenv').config();

const debug = require('debug')('app');
const { Client } = require('./Client');

const client = new Client(process.env.TOKEN);

client.connect().then(() => debug('connected')).catch(debug);

client.on('message', debug);
