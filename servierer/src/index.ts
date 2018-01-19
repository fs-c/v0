import * as Koa from 'koa';
import * as path from 'path';
import * as logger from 'debug';
import * as dotenv from 'dotenv';
import * as views from 'koa-views';

import * as router from './router';

dotenv.config();

const app = new Koa();
const debug = logger('app:server*');

app.on('error', debug);

app.use(views(path.join(__dirname, '/views'), { extension: 'ejs' }));

app.use(router.routes).use(router.allowedMethods);

const port = process.env.PORT || 8080;
const server = app.listen(port);
debug('listening on port %O', port);