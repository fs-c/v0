import { config } from 'dotenv';
config();

import * as logger from 'debug';
import { Client } from 'discord.js';
import { log } from './components/logger';
import { createInterface } from 'readline';
import { writeFileSync, readFileSync } from 'fs';

const client = new Client();
const debug = logger('app');

const token = process.env.TOKEN;
client.login(token);

client.on('ready', () => {
  debug(`ready`);
});
