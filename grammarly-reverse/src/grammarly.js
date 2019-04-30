const pino = require('pino');
const axios = require('axios');
const WebSocket = require('websocket');
const EventEmitter = require('events');

// TODO: Doesn't handle edge cases
const serializeCookies = (obj) => {
    let string = '';

    for (const name in obj) {
        string += `${name}=${JSON.stringify(obj[name])};`;
    }

    return string;
};

class Grammarly extends EventEmitter {
    constructor(options = {}) {
        super();

        this.options = Object.assign({
            cookies: {},
            logging: 'silent',
            client: new (WebSocket.client)(),
        }, options);

        this.temp  = [];

        this.connection = {};
        this.client = this.options.client;
        this.cookies = this.options.cookies;

        this.log = pino({
            base: null,
            name: 'grammarly',
            level: this.options.logging,
        });

        this.client.on('connectFailed', (err) => {
            this.log.error(err, `connect error`);

            this.emit('connectError', err);
        });

        this.client.on('connect', (connection) => {
            this.log.info('connection established')

            this.connection = connection;

            this.setupConnectionHandlers();
            this.handleInitial();

            this.emit('connected');
        });
    }

    get connectHeaders() { return {
        // Host and Origin are missing

        'Sec-WebSocket-Version': '13',
        // Sec-WebSocket-Key is missing

        'Cookie': serializeCookies({
            'isGrammarlyUser': true,
            // Changed FREEMIUM to PREMIUM, apparently they don't check this
            'browser_info': 'CHROME:67:COMPUTER:SUPPORTED:PREMIUM:WINDOWS_10:WINDOWS',
            'experiment_groups': 'gdpr_signup_enabled|gb_in_editor_free_Test1|'
                + 'google_docs_free_release_enabled',

            'grauth': this.cookies.grauth,
            'csrf-token': this.cookies.csrf,
            'gnar_containerId': this.cookies.gnarID,
        }),
    } }

    connect() { (async () => {
        const { headers } = await axios.get('https://grammarly.com');

        // TODO: Just look at it
        const cookies = (headers['set-cookie'] || [])
            .map((ckie) => ckie.split(';')[0])
            .reduce((acc, cur, i) => {
                const splt = cur.split('=');
                acc[splt[0]] = splt[1];
                return acc;
            }, {});

        this.cookies.grauth = cookies['grauth'];
        this.cookies.csrf = cookies['csrf-token'];
        this.cookies.gnarID = cookies['gnar_containerId'];

        this.log.trace(this.cookies, 'fetched cookies');

        this.client.connect('wss://capi.grammarly.com/freews', null,
            'https://app.grammarly.com', this.connectHeaders);
    })().catch((err) => {
        this.log.trace('connect error', err);

        this.emit('connectError', err);
    }) }

    handleInitial() {
        this.sendJSON({
            id: 1,
            token: "", // This is empty in the original hello as well
            action: "start",
            client: "denali_editor",
            clientSubtype: "general",
            // TODO: Could this be altered to include more features?
            clientSupports: [
                "text_info", "free_inline_advanced_alerts", "readability_check", 
                "sentence_variety_check", "filler_words_check", "alerts_update", 
                "alerts_changes", "free_clarity_alerts",
            ],
            clientVersion: "1.5.43-1577+master",
            dialect: "british",
            // Missing docid and documentContext
        });

        this.log.debug('sent initial message');
    }

    setupConnectionHandlers() {
        this.connection.on('error', (err) => {
            this.log.error(err, 'connection error');

            this.emit('connectionError', err);
        });

        this.connection.on('close', () => {
            this.log.info('connection closed');

            this.emit('disconnected');
        });

        this.connection.on('message', (msg) => {
            this.handleMessage(msg);
        });
    }

    handleMessage(msg) {
        if (msg.type !== 'utf8' || !msg.utf8Data) {
            this.log.debug('got binary message, aborting');

            return;
        }

        try {
            const data = JSON.parse(msg.utf8Data);

            this.log.trace({ action: data.action }, 'received message');

            this.handleData(data);
        } catch (err) {
            this.log.debug(err, 'got invalid message, aborting');

            return;
        }
    }

    handleData(data) {
        if (data.action === 'start') {
            this.emit('ready');

            this.temp = [];
        } else if (data.action === 'alert') {
            this.temp.push(data);
        } else if (data.action === 'finished') {
            this.emit('alerts', this.temp);

            this.temp = [];
        }
    }

    submitText(text) {
        this.sendJSON({
            id: 1,
            rev: 0, // What's this?
            doc_len: 0, // Seems to always be zero; obsolete?
            deltas: [
                { ops: [ { insert: text, }, ], },
            ],
            action: 'submit_ot',
        });

        this.log.trace('sent text delta');

        this.sendJSON({
            id: 3,
            action: 'option',
            name: 'gnar_containerId',
            value: this.cookies.gnarID,
        });

        this.log.trace('sent container id');

        this.log.debug('finished text submission');
    }

    sendJSON(data) {
        if (!this.connection.connected) {
            this.log.trace(`tried sending data through disconnected socket`);

            return;
        }

        const string = JSON.stringify(data);

        this.log.trace(string, `sending to ${this.connection.remoteAddress}`);

        this.connection.sendUTF(string);
    }
}

exports.Grammarly = Grammarly;
