import * as dotenv from 'dotenv';
dotenv.config();

import * as Koa from 'koa';
import * as log from 'debug';
import * as path from 'path';

const app = new Koa();
const debug = log('app');

// Config.
app.proxy = true; // Trust first proxy.
app.keys = [ process.env.SESSION_SECRET ];

// Error and dev style request logging.
import * as logger from 'koa-logger';
app.use(logger());
app.on('error', (err) => debug(err.message));

// Flash messages and query string parsing.
import flash from './scripts/flash';
import urlparser from './scripts/urlparser';
app.use(flash());
app.use(urlparser());

// Persistent session object in the context and request body parsing.
import * as session from 'koa-session';
import * as bodyParser from 'koa-bodyparser';
app.use(bodyParser());
app.use(session({}, app));

// Auth flow.
import { passport } from './config/passport';
app.use(passport.initialize());
app.use(passport.session());

// Render views from /views.
import * as views from 'koa-views';
app.use(views(
  path.join(__dirname, '/views'), {
    extension: 'ejs',
    options: {
      version: require('../package.json').version,
      node: process.version,
    },
  }),
);

// 'Handle' errors.
app.use(async (ctx, next) => {
  try {
    await next();
  } catch (err) {
    app.emit('error', err); // Preserve default behavior.

    ctx.status = ctx.status || 500;
    await ctx.render('status', {
      message: err.message,
    });
  }
});

// Add routes.
import router from './routes/router';
app.use(router.routes());
app.use(router.allowedMethods());

// Serve static assets.
import * as serve from 'koa-static';
app.use(serve(path.join(__dirname, '/public')));

// Start listening, export server for tests.
const port = parseInt(process.env.PORT, 10) || 8080;
export const server = app.listen(port);
debug('listening on port %o', port);

// Connect to database.
import * as mongoose from 'mongoose';

const dbURL = process.env.DB_URL;
const dbPass = process.env.DB_PASS;
const dbUser = process.env.DB_USER;

if (dbURL && dbUser && dbPass) {
  mongoose.connect(`mongodb://${dbUser}:${dbPass}@${dbURL}`)
    .then(() => debug('connected to database'))
    .catch((err) => debug('database connection error: %O', err.message));
} else {
  throw new Error('Missing database connection parameters.');
}
