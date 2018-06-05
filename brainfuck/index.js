const { stdin, stdout } = require('process');

stdin.setEncoding('utf8');

const putChar = (c, e = 'utf8') => stdout.write(c, e);
const getChar = () => new Promise((resolve) => {
    stdin.once('data', (chunk) => resolve(chunk));    
});

(async () => {



})().catch(console.error);