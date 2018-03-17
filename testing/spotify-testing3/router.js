const { STATE_KEY } = global;

const request = require('request');
const querystring = require('querystring');
const router = module.exports = new (require('koa-router'))();

function generateRandomString(length) {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

  let text = '';
  for (var i = 0; i < length; i++) {
    text += chars.charAt(Math.floor(Math.random() * chars.length));
  }

  return text;
}

router.get('/login', (ctx, next) => {
  const state = generateRandomString(16);

  ctx.cookies.set(STATE_KEY, state);

  const scope = 'user-top-read';
  const clientID = process.env.CLIENT_ID;

  const url = 'https://accounts.spotify.com/authorize?'
    + querystring.stringify({
        scope,
        state,
        client_id: clientID,
        response_type: 'code',
        redirect_uri: ctx.origin + '/callback',
      })

  console.log('redirecting to ' + url);

  ctx.redirect(url);
});

router.get('/callback', (ctx, next) => {
  const { code, state } = ctx.request.query;
  const storedState = ctx.cookies.get(STATE_KEY);

  const clientID = process.env.CLIENT_ID;
  const clientSecret = process.env.CLIENT_SECRET;

  if (!state || state !== storedState) {
    return ctx.throw('State mismatch.');
  } else {
    ctx.cookies.set(STATE_KEY, null, { overwrite: true });

    request.post({
      url: 'https://accounts.spotify.com/api/token',
      form: {
        code,
        redirect_uri: ctx.origin + '/callback',
        grant_type: 'authorization_code',
      },
      headers: {
        'Authorization': 'Basic '
          + (new Buffer(`${clientID}:${clientSecret}`).toString('base64')),
      },
      json: true,
    }, (err, res, body) => {
      console.log('got response from token post');
      console.log(body.access_token);

      if (err || res.statusCode !== 200) {
        return ctx.throw('Invalid token.');
      }

      const url = ctx.origin + '/#' + querystring.stringify({
          access_token: body.access_token,
          refresh_token: body.refresh_token,
        })

      console.log('post-redirecting to ' + url);

      ctx.redirect(url);
    });
  }
});
