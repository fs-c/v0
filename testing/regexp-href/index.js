const html = require('fs').readFileSync(0, 'utf8');

const regex = /(?<=<a(.*?)href=(["'"]))(.*?)(?=(["']))/g;
const match = html.match(regex);

console.log(match);