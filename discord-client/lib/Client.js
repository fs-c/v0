const WebSocket = require('ws');
const EventEmitter = require('events');
const debug = require('debug')('discord');

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
   * @param {string} token - The token to use when identifying.
   * @param {Object} options - Connection options.
   * @param {number} options.version - The Discord Gateway Version to use.
   * @param {number} options.interval - The default heartbeat interval.
   * @param {number} options.maxDelay - The max delay between heartbeat and ACK.
   */
  constructor(token, options = {}) {
    super();

    if (!token) {
      throw new Error('No token provided');
    }

    this.options = Object.assign({
      version: 6,
      interval: 15 * 1000,
      maxDelay: 15 * 1000,
    }, options);

    this.socket;

    this.token = token;
    this.readyState = 'disconnected';

    this.gateway = {
      encoding: 'json',
      version: this.options.version,
      baseURL: 'wss://gateway.discord.gg/',
      get url() {
        return `${this.baseURL}?v=${this.version}&encoding=${this.encoding}`;
      },
    };

    this.connection = {
      sequence: 0,                  // Allow for persistence across disconnects.
      ping: undefined,              // Ms delay between heartbeat and ACK.
      sessionID: undefined,
    };

    this.heartbeat = {
      lastSent: 0,                  // Time when last heartbeat was sent.
      timer: undefined,
      interval: options.interval,   // Should be overwritten by server response.
      maxDelay: options.maxDelay,
      timeoutTimer: undefined,      // Detect zombied connection.
    };
  }

  get state() { return this.readyState; }
  set state(state) {
    debug('state %o -> %o', this.readyState, state);

    this.emit('state', this.readyState, state);

    this.readyState = state;
  }

  /**
   * Initializes a connection to the Discord WS servers and pass messages
   * to handler.
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

    // Forward error, let user decide what to do with it.
    this.socket.on('error', (err) => this.emit('error', err));
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
   * The callback for sending messages to Discord.
   * 
   * @callback sendCallback
   * @param {(Error|undefined)} err - Defined if an error occured.
   */

  /**
   * Send a message to the Discord WS server.
   * 
   * @param {(string|buffer)} data - The data to send.
   * @param {sendCallback} [cb] - Callback once data is sent.
   * @public
   */
  send(data, cb) {
    debug('attempting to send something');

    cb = typeof cb === 'function' ? cb : () => {};

    if (this.socket || this.socket.readyState !== 1) {
      return cb(new Error('Socket not ready'));
    }

    this.socket.send(data, (err) => {
      if (err) {
        debug('failed sending message (%o)', err.message);

        this.emit('error', err);

        return cb(err);
      } else { return cb(); }
    });
  }
};

// Handle incoming websocket messages.
Client.prototype.handle = require('./handle');
