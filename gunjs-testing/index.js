const Koa = require('koa');
const Gun = require('gun');

const app = new Koa();

const serve = require('koa-static');
app.use(serve(require('path').join(process.cwd(), '/public')));

app.use(Gun.serve);

const server = app.listen(8080);
const gun = Gun({ file: 'data.json', web: server });
