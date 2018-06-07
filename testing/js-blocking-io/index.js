const { stdin } = require('process');

stdin.setEncoding('utf8');

const instack = [];
const getChar = () => new Promise((resolve) => {
    if (instack.length > 0) {
        resolve(instack.pop());
    }

    stdin.once('data', (chunk) => {
        instack.push(...chunk.trim().split(''));
    
        resolve(instack.pop());
    });
});

(async () => {

let a = await getChar();

console.log(a);

let b = await getChar();

console.log(a, b);

})().catch(console.error);
