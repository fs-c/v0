const WebSocket = require('ws');
const EventEmitter = require('events');
const debug = require('debug')('discord');

const { payloads } = require('./payloads');

require('debug').enable('discord');

/**
 * Class representing a Discord Client.
 *
 * @extends EventEmitter
 */
const Client = module.exports = class extends EventEmitter {
  /**
   * Create a new `Client`.
   *
   * @param {String} token The token to use when identifying
   * @param {Object} options Connection options
   * @param {Number} options.version The Discord Gateway Version to use
   * @param {Number} options.interval The default heartbeat interval
   * @param {Number} options.maxDelay The max delay between heartbeat and ACK
   */
  constructor(token, options = {}) {
    super();

    if (!token) {
      throw new Error('No token provided');
    }

    this.options = Object.merge({
      version: 6,
      interval: 15 * 1000,
      maxDelay: 15 * 1000,
    }, options);

    this.socket;

    this.token = token;
    this.readyState = 'disconnected';

    this.gateway = {
      version: options.version,
      url: `wss://gateway.discord.gg/?v=${options.version}&encoding=json`,
    }

    this.connection = {
      sequence: 0,                  // Allow for persistence across disconnects.
      ping: undefined,              // Ms delay between heartbeat and ACK.
      sessionID: undefined,
    }

    this.heartbeat = {
      lastSent: 0,                  // Time when last heartbeat was sent.
      timer: undefined,
      interval: options.interval,   // Should be overwritten by server response.
      maxDelay: options.maxDelay,
      timeoutTimer: undefined,      // Detect zombied connection.
    }
  }

  get state() { return this.readyState }
  set state(state) {
    debug('state %o -> %o', this.readyState, state);

    this.emit('state', this.readyState, state);

    this.readyState = state;
  }

  /**
   * Initializes a connection to the Discord WS servers and starts the 
   * keep-alive process.
   *
   * @public
   */
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

  /**
   * Close connection, restore Client to `disconnected` state.
   *
   * @public
   */
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

  /**
   * Send a message to the Discord WS.
   * 
   * @param {*} data The data to send
   * @param {Function} cb Callback
   * @public
   */
  send(data, cb) {
    cb = typeof cb === 'function' ? cb : () => {};

    if (this.socket || this.socket.readyState !== 1) {
      return callback(new Error('Socket not ready'));
    }

    this.socket.send(data, (err) => {
      if (err) {
        debug('failed sending message (%o)', err.message);

        this.emit('error', err);

        return callback(err);
      } else { return callback() }
    });
  }
}

// Handle incoming websocket messages.
Client.prototype.handle = require('./handle');
