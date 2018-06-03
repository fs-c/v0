require('dotenv').config();

const inDev = process.env.NODE_ENV !== 'production';

const _ = require('koa-route');
const app = new (require('koa'))();
const { URLSearchParams } = require('url');
// TODO: Replace this bloated mess.
const { post } = require('request-promise-native');

let debug = () => {};
if (inDev) {
    app.use(require('koa-logger')());

    const dbg = require('debug');
    debug = dbg('nowplaying');
    dbg.enable('nowplaying');
}

const temp = {};

const client_id = process.env.CLIENT_ID;
const redirect_uri = process.env.REDIRECT_URI;
const client_secret = process.env.CLIENT_SECRET;

const generateString = (length) => {
    let text = '';
    let alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        + 'abcdefghijklmnopqrstuvwxyz0123456789';
    
    for (let i = 0; i < length; i++) {
        text += alphabet[Math.floor(Math.random() * alphabet.length)];
    }

    return text;
}

app.use(require('@koa/cors')());
app.use(require('koa-static')('public', { maxage: inDev ? 0 : 60 * 1000 }));

app.use(async (ctx, next) => {
    try {
        await next();
    } catch (err) {
        debug(err);
    }
});

app.use(_.get('/from/:id', async (ctx, id, next) => {
    const data = temp[id];

    ctx.type = 'application/json';
    ctx.body = data;

    return;
}));

app.use(_.get('/add', async (ctx, next) => {
    const state = generateString(16);
    const scope = 'user-read-currently-playing user-read-playback-state';

    temp[state] = true;

    return ctx.redirect('https://accounts.spotify.com/authorize?' +
        new URLSearchParams({
            scope,
            state,
            client_id,
            redirect_uri,
            client_secret,
            response_type: 'code',
        }).toString()
    );
}));

app.use(_.get('/callback', async (ctx, next) => {
    const { code, state } = ctx.query;

    if (!temp[state]) {
        ctx.status = '403';
        return ctx.body = 'ERROR: State mismatch.';
    }

    const auth = new Buffer(client_id + ':' + client_secret)
        .toString('base64');

    const res = await post('https://accounts.spotify.com/api/token', {
        json: true,
        headers: {
            'Authorization': 'Basic ' + auth,
        },
        form: {
            code,
            redirect_uri,
            grant_type: 'authorization_code',
        },
    });

    temp[state] = res;

    ctx.redirect('/from/' + state);
}));

app.listen(8080);
