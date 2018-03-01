const WS_URL = 'wss://gateway.discord.gg/?v=6&encoding=json';

const WebSocket = require('ws');
const debug = require('debug')('app');

const ws = new WebSocket(WS_URL);

let sessionID;
let sequence = 0;

ws.on('open', () => debug('opened'));
ws.on('close', () => debug('disconnected'));

ws.on('message', (data) => {
  debug('received message');

  const message = JSON.parse(data);

  switch (message.op) {
    case 0: sequence++;
      break;
    case 9:
      debug('invalid session');
      break;
    case 10:

  }
});
