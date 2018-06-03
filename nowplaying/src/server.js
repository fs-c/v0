require('dotenv').config();

const inDev = process.env.NODE_ENV !== 'production';

const _ = require('koa-route');
const app = new (require('koa'))();
const { URLSearchParams } = require('url');
// TODO: Replace this bloated mess.
const { get, post } = require('request-promise-native');

let debug = () => {};
if (inDev) {
    app.use(require('koa-logger')());

    const dbg = require('debug');
    debug = dbg('nowplaying');
    dbg.enable('nowplaying');
}

const temp = {};

const redirectURI = process.env.REDIRECT_URI;
const client = {
    id: process.env.CLIENT_ID,
    secret: process.env.CLIENT_SECRET,
}

const auth = new Buffer(client.id + ':' + client.secret)
    .toString('base64');

const generateString = (length) => {
    let text = '';
    let alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        + 'abcdefghijklmnopqrstuvwxyz0123456789';
    
    for (let i = 0; i < length; i++) {
        text += alphabet[Math.floor(Math.random() * alphabet.length)];
    }

    return text;
}

const updateToken = async (id, auth, code, redirect, refresh) => {
    debug('updating token for %o', id);

    const res = await post('https://accounts.spotify.com/api/token', {
        json: true,
        headers: {
            'Authorization': 'Basic ' + auth,
        },
        form: {
            redirect_uri: redirect,
            [refresh ? 'refresh_token' : 'code']: code,
            grant_type: refresh ? 'refresh_token' : 'authorization_code',
        },
    });

    res.updated = Date.now();

    return temp[id] = res;
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
    let data = temp[id];

    if (!data) {
        ctx.status = 404;
        return ctx.body = 'ERROR: Invalid id.'
    }

    if (data.updated + (data.expires_in * 1000) < Date.now()) {
        data = await updateToken(id, auth, data.refresh_token, null, true);
    }

    const playback = await get('https://api.spotify.com/v1/me/player', {
        json: true,
        headers: {
            'Authorization': 'Bearer ' + data.access_token,
        },
    });

    const item = playback.item;
    const { album } = item;
    const artist = item.artists[0];

    ctx.type = 'application/json';
    ctx.body = `Listening to ${artist.name} - ${item.name}.`;

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
            response_type: 'code',
            redirect_uri: redirectURI,
            client_id: client.id,
            client_secret: client.secret,
        }).toString()
    );
}));

app.use(_.get('/callback', async (ctx, next) => {
    const { code, state } = ctx.query;

    if (!temp[state]) {
        ctx.status = '403';
        return ctx.body = 'ERROR: State mismatch.';
    }

    await updateToken(state, auth, code, redirectURI);

    ctx.redirect('/from/' + state);
}));

app.listen(8080);
