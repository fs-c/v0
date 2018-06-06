const { stdin, stdout } = require('process');

stdin.setEncoding('utf8');
stdout.setEncoding('utf8');

const putChar = (c, e = 'utf8') => stdout.write(c.toString().trim() + '\n', e);

let instack = [];
const getChar = () => new Promise((resolve) => {
    if (instack.length > 0) {
        resolve(instack.pop());
    }

    stdin.once('data', (chunk) => {
        instack.push(...chunk.trim().split(''));
    
        resolve(instack.pop());
    });
});

let brainfuck = ', + .';

let i = 0;
let ptr = 0;
let skipign = 0;
let skip = false;

const loops = [];
const register = [];

const perform = async (op) => {
    switch (op) {
    case '>':
        !skip ? ptr++ : 0;
        break;
    case '<':
        !skip ? ptr -= ptr <= 0 ? 0 : 1 : 0;
        break;
    case '+':
        !skip ? register[ptr] = register[ptr] ? register[ptr] + 1 : 1 : 0;
        break;
    case '-':
        !skip ? register[ptr] -= register[ptr] <= 0 ? 0 : 1 : 0;
        break;
    case ',':
        !skip ? register[ptr] = (await getChar()).charCodeAt(0) : 0;
        break;
    case '.':
        !skip ? putChar(String.fromCharCode(register[ptr])) : 0;
        break;
    case '[':
        skip ? skipign++ : ptr ? loops.push({ i, ptr }) : skip = true;
        break;
    case ']':
        if (skip && --skipign <= 0) {
            skip = false, skipign = 0;
        } else {
            i = loops.pop().i;
        }

        break;
    default:
        // ???
        break;
    }
}

(async () => {

while (i <= brainfuck.length) {
    await perform(brainfuck[i++]);
}

console.log(register);

})().catch(console.error);
