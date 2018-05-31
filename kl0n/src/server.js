require('dotenv').config();

const app = new (require('koa'))();
const log = require('./logger')('server');

app.keys = [ process.env.SESSION_SECRET ];

app.use(async (ctx, next) => {
    try {
        await next();
    } catch(err) {
        log.warn(err);
    }
})

app.use(require('koa-bodyparser')());
app.use(require('koa-session')({}, app));

// TODO: Why does destructuring ({} = ...) break this?
const passport = require('./auth');
app.use(passport.initialize()).use(passport.session());

const { routes, allowedMethods } = require('./routes');
app.use(routes()).use(allowedMethods());

app.listen(8080);
