const Discord = require('../discord-client');

const client = new Discord.Client();

const readline = require('readline');
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  completer: (line, callback) => {
    const hits = [];

    callback(null, [ hits, line ]);
  }
});

rl.on('line', (line) => {});
