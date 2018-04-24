const app = new (require('koa'))();

app.use(async (ctx) => {
  const chapters = require('./chapters');
  const content = chapters.map((chap) => chap.content).join('<br>');

  ctx.body = `
  <html>
    <head>
      <title>rrl-concat</title>
    </head>

    <body>
      ${content}
    </body>
  </html>
  `
});

app.listen(8080);