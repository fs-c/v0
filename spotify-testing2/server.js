require('dotenv').config();

const app = new (require('koa'))();

app.use(require('koa-static')('public'));

const server = app.listen(process.env.PORT || '8080');