const pino = require('pino');
const WebSocket = require('websocket');
const EventEmitter = require('events');

// Doesn't handle edge cases so use with caution
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

        this.temp  = [];

        this.connection = {};
        this.cookies = options.cookies || {};
        this.client = options.client || new (WebSocket.client)();

        this.log = options.logging ? pino({
            base: null,
            name: 'grammarly',
            loglevel: options.logging,
        }) : () => {};

        this.client.on('connectFailed', (err) => {
            this.log.error(err, `connect error`);
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
        'Host': 'capi.grammarly.com',
        'Origin': 'https://app.grammarly.com', // Obsolete?

        'Sec-WebSocket-Version': '13',
        // Sec-WebSocket-Key is missing

        'Cookie': serializeCookies({
            'isGrammarlyUser': true,
            'browser_info': 'CHROME:67:COMPUTER:SUPPORTED:FREEMIUM:WINDOWS_10:WINDOWS',
            'experiment_groups': 'gdpr_signup_enabled|gb_in_editor_free_Test1|'
                + 'google_docs_free_release_enabled',

            'grauth': this.cookies.grauth,
            'csrf-token': this.cookies.csrf,
            'gnar_containerId': this.cookies.gnarID,
        }),
    } }

    connect() { (async () => {
        const { headers } = await axios.get('https://grammarly.com');

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
        this.emit('connectError', err);
    }) }

    handleMessage(msg) {
        if (msg.type !== 'utf8' || !msg.utf8Data) {
            this.log.debug('got binary message, aborting');

            return;
        }

        try {
            const data = JSON.parse(msg.utf8Data);

            this.handleData(data);
        } catch (err) {
            this.log.debug(err, 'parsing error');

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

    handleInitial() {
        this.connection.sendUTF(JSON.stringify({
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
        }));

        this.log.trace('sent initial message');
    }

    submitText(text) {
        this.connection.sendUTF(JSON.stringify({
            id: 1,
            rev: 0, // What's this?
            doc_len: 0, // Seems to always be zero; obsolete?
            deltas: [
                { ops: [ { insert: text, }, ], },
            ],
            action: 'submit_ot',
        }));

        log.debug('sent text delta');
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
            this.log.trace('received message');

            this.handleMessage(msg);
        });
    }
}

exports.Grammarly = Grammarly;
