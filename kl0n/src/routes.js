const passport = require('koa-passport');
const router = module.exports = new (require('koa-router'))();

const isAuthenticated = async (ctx, next) => {
    if (ctx.isAuthenticated()) {
        await next();
    }

    res.redirect('/login');
}

router.get('/', isAuthenticated, async (ctx) => {
    ctx.body = 'hello there';
});

const { AZ_DOMAIN, AZ_CALLBACK, AZ_CLIENT_ID, AZ_CLIENT_SECRET } = process.env;
router.get('login', passport.authenticate('auth0', {
    clientID: AZ_CLIENT_ID,
    domain: AZ_DOMAIN,
    redirectUri: AZ_CALLBACK,
    audience: `https://${AZ_DOMAIN}/userinfo`,
    responseType: 'code',
    scope: 'openid',
}), async (ctx) => {
    ctx.redirect('/');
});

router.get('/logout', async (ctx) => {
    ctx.logout();
    ctx.redirect('/');
});

router.get('/callback', passport.authenticate('auth0', {
    failureRedirect: '/login',
}), async (ctx) => {
    ctx.redirect('/');
});
