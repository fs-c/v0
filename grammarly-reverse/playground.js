const axios = require('axios');
const client = new (require('websocket').client)();

const log = require('pino')({
    base: null,
    name: 'grmly',
    level: 'trace',
});

(async () => {

// Doesn't handle edge cases so use with caution
const serializeCookies = (obj) => {
    let string = '';

    for (const name in obj) {
        string += `${name}=${JSON.stringify(obj[name])}; `;
    }

    return string;
};

try {
    const { headers } = await axios.get('https://grammarly.com');

    const cookies = (headers['set-cookie'] || [])
        .map((ckie) => ckie.split(';')[0])
        .reduce((acc, cur, i) => {
            const splt = cur.split('=');
            acc[splt[0]] = splt[1];
            return acc;
        }, {});

    process.env.COOKIE_GRAUTH = cookies['grauth'];
    process.env.COOKIE_CSRF = cookies['csrf-token'];
    process.env.COOKIE_GNAR_ID = cookies['gnar_containerId'];
} catch (err) {
    log.error(err, 'failed request, aborting');

    process.exit(1);
}

client.on('connectFailed', (err) => {
    log.error(err, `connect error`);
});

const sendJSON = (conn, data) => {
    const string = JSON.stringify(data);

    log.trace(string, `sending to ${conn.remoteAddress}`);

    conn.sendUTF(string);
};

const sendInitial = (conn) => {
    sendJSON(conn, {
        id: 1,
        token: "", // This is empty in the original hello as well
        action: "start",
        client: "denali_editor",
        clientSubtype: "general",
        clientSupports: [
            "text_info", "free_inline_advanced_alerts", "readability_check", 
            "sentence_variety_check", "filler_words_check", "alerts_update", 
            "alerts_changes", "free_clarity_alerts",
        ],
        clientVersion: "1.5.43-1577+master",
        dialect: "british",
        // Missing docid and documentContext
    });

    log.info('sent hello');
};

const sendData = (conn, text) => {
    sendJSON(conn, {
        id: 1,
        rev: 0, // What's this?
        doc_len: 0, // This is zero in the original request as well, seems odd
        deltas: [
            { ops: [ { insert: text, }, ], },
        ],
        action: 'submit_ot',
    });

    log.debug('sent delta');

    sendJSON(conn, {
        id: 3,
        action: 'option',
        name: 'gnar_containerId',
        value: process.env.COOKIE_GNAR_ID,
    });

    log.debug('sent container id');

    log.info('finished data transmission');
};

const handleAlert = (data) => {
    log.debug(`'${data.categoryHuman}' from ${data.begin} to ${data.end}: '${data.text}'`);
};

client.on('connect', (conn) => {
    log.info('connection established');

    conn.on('error', (err) => {
        log.error(err, `connection error`);
    });

    conn.on('close', () => {
        log.info('connection closed');
    });

    conn.on('message', (msg) => {
        log.trace(msg, 'received message');

        if (msg.type !== 'utf8') {
            log.warn('got binary data, aborting');

            return;
        }

        try {
            const data = JSON.parse(msg.utf8Data);

            if (data.action === 'start') {
                return sendData(conn, 'Henlo my englisch is not so god');
            } else if (data.action === 'alert') {
                return handleAlert(data);
            }
        } catch (err) {
            log.error(err, 'parsing error');
        }
    });

    sendInitial(conn);
});

const connectHeaders = {
    'Host': 'capi.grammarly.com',
    'Origin': 'https://app.grammarly.com', // Obsolete?

    'Sec-WebSocket-Version': '13',
    // Sec-WebSocket-Key is missing

    'Cookie': serializeCookies({
        'isGrammarlyUser': true,
        'browser_info': 'CHROME:67:COMPUTER:SUPPORTED:FREEMIUM:WINDOWS_10:WINDOWS',
        'experiment_groups': 'gdpr_signup_enabled|gb_in_editor_free_Test1|'
            + 'google_docs_free_release_enabled',

        'grauth': process.env.COOKIE_GRAUTH,
        'csrf-token': process.env.COOKIE_CSRF,
        'gnar_containerId': process.env.COOKIE_GNAR_ID,
    }),
};

client.connect('wss://capi.grammarly.com/freews', null,
    'https://app.grammarly.com', connectHeaders, null);

})();
