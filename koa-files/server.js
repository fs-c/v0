const Koa = require('koa');
const app = new Koa();

const files = require('./');
app.use(files());

const server = app.listen(8080);