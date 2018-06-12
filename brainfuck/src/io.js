const { stdin, stdout } = require('process');

stdin.setEncoding('utf8');
stdout.setEncoding('utf8');

const putChar = exports.putChar = (c, e = 'utf8') =>
    stdout.write(c.toString().trim() + '\n', e);

let instack = [];
const getChar = exports.getChar = () => new Promise((resolve) => {
    if (instack.length > 0) {
        resolve(instack.pop());
    }

    stdin.once('data', (chunk) => {
        instack.push(...chunk.trim().split(''));
    
        resolve(instack.pop());
    });
});

// TODO: Why does the following break IntelliSense?
//       Investigate fix.
// Object.assign(exports, { putChar, getChar });
