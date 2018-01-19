import * as fs from 'fs';
import * as path from 'path';
import * as Router from 'koa-router';

const router = new Router();

router.get('/', async (ctx) => {
  ctx.status = 200;  
  ctx.type = 'html';
  ctx.body = fs.createReadStream(path.join(__dirname, 'public/index.html'));
});

export const routes = router.routes();
export const allowedMethods = router.allowedMethods();