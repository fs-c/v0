const forward = require('fwd');
const WebSocket = require('ws');
const EventEmitter = require('events');
const debug = require('debug')('discord');

const { payloads } = require('./payloads');

require('debug').enable('discord');

const Client = module.exports = class extends EventEmitter {
  constructor(token, options = {}) {
    if (!token) {
      throw new Error('No token provided.');
    }

    options.version = options.version || 6;

    super();

    this.socket;

    this.token = token;
    this.readyState = 'disconnected';

    this.gateway = {
      version: options.version,
      url: `wss://gateway.discord.gg/?v=${options.version}&encoding=json`,
    }

    this.connection = {
      sequence: 0,              // Allow for persistence across disconnects.
      ping: undefined,          // Ms delay between heartbeat and ACK.
      sessionID: undefined,     // Gotten from 'READY' event.
    }

    this.heartbeat = {
      lastSent: 0,              // Last sent heartbeat, to calculate ping.
      timer: undefined,         // The heartbeat timer.
      interval: 15 * 1000,      // Heartbeat interval; this will be overwritten.
      maxDelay: 15 * 1000,      // Max ms delay between heartbeat and ACK.
      timeoutTimer: undefined,  // Detect dead or zombied connection.
    }
  }

  get state() { return this.readyState }
  set state(state) {
    debug('state %o -> %o', this.readyState, state);

    this.emit('status', this.readyState, state);

    this.readyState = state;
  }

  // Connect to discord WS server and initialise keepalive process.
  connect() {
    if (this.state !== 'disconnected') {
      return new Error(`Invalid state (${this.state})!`);
    }

    this.state = 'connecting';

    this.socket = new WebSocket(this.gateway.url);

    this.socket.on('message', this.handle);

    this.socket.on('open', () => {
      this.state = 'connected';
    });

    this.socket.on('close', () => {
      this.state = 'disconnected';
    });

    // Forward error, let user decide what to do with the information.
    this.socket.on('error', (err) => this.send('error', err));
  }

  // Restore Client to original 'disconnected' state.
  disconnect() {
    this.state = 'disconnecting';

    if (this.socket.readyState <= 1) {
      this.socket.close();
    }

    // Clear timers.
    clearInterval(this.heartbeat.timer);
    clearTimeout(this.heartbeat.timeoutTimer);

    // Reset connection.
    this.connection.sequence = 0;    
    this.connection.sessionID = undefined;
    
    // Reset heartbeat vars.
    this.heartbeat.lastSent = 0;
    this.heartbeat.timer = undefined;
    this.heartbeattimeoutTimer = undefined;

    this.state = 'disconnected';
  }

  // Send data over the socket.
  send(data) {
    if (this.socket || this.socket.readyState !== 1) {
      return;
    }

    this.socket.send(data, (err) => {
      debug('failed sending message (%o)', err.message);

      this.emit('error', err)
    });
  }
}

// Handle incoming websocket messages.
Client.prototype.handle = require('./handle');
