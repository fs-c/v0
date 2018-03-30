const EventEmitter = require('events');

const Steam = require('steam-user');
const Community = require('steamcommunity');
const Manager = require('steam-tradeoffer-manager');

const fwd = require('fwd');
const { question } = require('readline-sync');
const { generateAuthCode } = require('steam-totp');
const { writeFile, existsSync, readFileSync } = require('fs');

const debug = require('debug')('trader');

const Trader = exports.Trader = class extends EventEmitter {
  /**
   * Construct a new trader.
   * 
   * @param {object} account - An account object.
   * @param {string} account.accountName - The username of the account.
   * @param {string} account.password - The plaintext password of the account.
   * @param {string} [account.shasec] - The shared secret of the account.
   * @param {string} [account.idsec] - The identity secret of the account.
   * @param {object} [options] - An options object.
   * @param {boolean} [options.autostart=false] - Start login upon construction.
   * @param {boolean} [options.queryCode=false] - Ask the user for their steam guard code.
   * @param {number} [options.confirmationInterval=15000] - The interval at which trades should be confirmed.
   */
  constructor(account, options = {}) {
    super();

    this.account = account;

    this.options = Object.assign({
      verbose: false,
      autostart: false,
      queryCode: false,
      confirmationInterval: 15000,
    }, options);

    this.client = new Steam();
    this.community = new Community();
    this.manager = new Manager({
      language: 'en',
      steam: this.client,
      pollInterval: 5000,
      domain: process.env.DOMAIN || 'example.com',
    });

    this.client.setOption('promptSteamGuardCode', false);

    if (existsSync('polldata.json')) {
      manager.pollData = JSON.parse(readFileSync('polldata.json', 'utf8'));
    }

    this.manager.on('polldata', (data) => {
      debug('got polldata');

      writeFile('polldata.json', JSON.stringify(data));
    });

    this.client.on('error', this.handleClientError.bind(this));
    this.client.on('steamGuard', this.handleSteamGuard.bind(this));
    this.client.on('webSession', this.handleWebSession.bind(this));

    fwd(this.manager, this);

    if (this.options.autostart) {
      this.client.logOn(account);
    }
  }

  static get EResult() { return Manager.EResult; }
  static get ETradeOfferState() { return Manager.ETradeOfferState; }

  /**
   * @param {Error} err
   * @returns {object} - The formatted error.
   * @private
   */
  static formatError(err) {
    return {
      err,
      // Common fatal error reasons.
      isFatal: [ 3, 5, 18, 84 ].includes(err.eresult),
      message: err.eresult ? Steam.EResult[err.eresult] : null,
    }
  }

  /**
   * @param {Error} err
   * @param {number} [err.eresult] - A steam EResult.
   * @private
   */
  handleClientError(err) {
    debug('client error (%o / %o)', err.message, err.eresult);

    this.emit('clientError', Trader.formatError(err));
  }

  /** 
   * @param {boolean} domain - Email code required.
   * @param {Function} callback - To call with the obtained Steam Guard code.
   * @private
   */
  handleSteamGuard(domain, callback) {
    debug('steam guard event (%o)', domain ? 'email' : 'mobile');

    const code = this.account.shasec ? (
      generateAuthCode(this.account.shasec)
    ) : this.options.queryCode ? (
      question(`${domain ? 'Email' : 'Mobile'} code: `)
    ) : undefined;

    if (!code) {
      return console.log('Failed to obtain Steam Guard code.');
    }

    return callback(code);
  }

  /**
   * @param {string} sessionID 
   * @param {object} cookies
   * @private
   */
  handleWebSession(sessionID, cookies) {
    debug('websession obtained');

    this.manager.setCookies(cookies, (err) => {
      if (err) {
        return this.emit('managerError', Trader.formatError(err));
      }

      debug('ready');

      return this.emit('ready');
    });

    this.community.setCookies(cookies);

    if (this.account.idsec) {
      const interval = this.options.confirmationInterval;

      this.community.startConfirmationChecker(
        interval, this.account.idsec
      )

      debug('set up confirmation checker at a %oms interval', interval);
    }
  }

  /**
   * Log on to steam and kick off the initialisation, 
   * resolves when the connection is established.
   * 
   * @returns {Promise} - Connection details.
   * @public
   */
  initialise() {
    return new Promise((resolve, reject) => {
      if (this.client.publicIP) {
        return resolve();
      }

      this.client.logOn(this.account);

      this.client.on('loggedOn', resolve);
    });
  }

  /**
   * Get all active (= outstanding) trade offers of our account.
   * 
   * @returns {Promise} - An array of offers.
   * @public
   */
  getOffers() {
    return new Promise((resolve, reject) => {
      this.manager.getOffers(1, (err, sent, received) => {
        if (err) {
          this.emit('managerError', Trader.formatError(err));

          return reject(err);
        }

        debug('got offers (%o)', sent.length + received.length);

        return resolve(sent.concat(received));
      });
    });
  }

  // This whole thing is inefficient and it makes my skin crawl.
  // TODO: Does the object structure provide any sort of performance boost?
  //       Would a Set be worth it?
  /**
   * Return an offer given an ID or a fraction of one.
   * 
   * @param {string} partial - A potentially incomplete offer ID.
   * @returns {object}
   */
  async getOffer(partial) {
    const offers = await this.getOffers().reduce((acc, cur) => {
      acc[cur.id] = cur;

      return acc;
    }, {});

    if (offers[partial]) {
      return offers[partial];
    }

    // This is just stupid.
    const offerID = Object.keys(offers).reduce((acc, id) => {
      const dist = levenshtein.get(partial, id);

      if (dist < acc.dist) {
        acc = { dist, id };
      }

      return acc;
    }, { dist: Infinity, id: null }).id;

    return offers[offerID];
  }
}