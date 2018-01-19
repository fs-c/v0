import * as Koa from 'koa';
import * as log from 'debug';
import * as path from 'path';
import * as dotenv from 'dotenv';
import * as views from 'koa-views';
import * as mongoose from 'mongoose';
import * as logger from 'koa-logger';
import * as session from 'koa-session';
import * as passport from 'koa-passport';
import * as bodyParser from 'koa-bodyparser';

import { routes, allowedMethods } from './router';

// Load env vars from .env.
dotenv.config();

const app = new Koa();
const debug = log('app:server');

// Settings.
app.proxy = true;
app.keys = [ process.env.SESSION_SECRET ];

// Logging.
app.on('error', (err) => debug(err.message));
app.use(logger());

// Basic needs.
app.use(session({}, app));
app.use(bodyParser());

// Init passport.
app.use(passport.initialize());
app.use(passport.session());

// Set headers.
app.use(async (ctx, next) => {
  const start = Date.now();
  await next();
  ctx.set('X-Response-Time', `${Date.now() - start}ms`);
});

// Render views.
app.use(views(path.join(__dirname, '/views'), { extension: 'ejs' }));

// Error handler.
app.use(async (ctx, next) => {
  try {
    await next();
  } catch(err) {
    app.emit('error', err);
    
    ctx.status = ctx.status || 500;
    await ctx.render('error', { err })
  }
});

// Use routes
app.use(routes).use(allowedMethods);

// Start server, export for tests.
const port = process.env.PORT || 8080;
export const server = app.listen(port);
debug('listening on port %O', port);

// Connect to database.
const dbURL = process.env.DB_URL;
const dbPass = process.env.DB_PASS;
const dbUser = process.env.DB_USER;

if (dbURL && dbUser && dbPass) {
  mongoose.connect(`mongodb://${dbUser}:${dbPass}@${dbURL}`);
} else throw new Error('Missing database connection parameters.');

const db = mongoose.connection;
db.on('error', (err) => debug(`database connection error: %O`, err));
db.once('connected', () => debug(`connected to database`));