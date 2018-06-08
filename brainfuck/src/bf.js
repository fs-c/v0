const { getChar, putChar } = require('./io');

// Position in the instructions.
let i = 0;

// "Pointer" to current active cell.
let ptr = 0;
// Cells.
const cls = [];

/**
 * Find the index of the closing sequence for a matching opening sequence in a
 * source.
 */
const find = (source, opening, closing, backwards = false) => {
    let igc = 0;

    source = backwards ? source.split('').reverse().join('') : source;

    for (let i = 0; i < source.length; i++) {
        const c = source[i];

        if (c === opening) {
            igc++;
        } else if (c === closing) {
            if (--igc <= 0) {
                return backwards ? (source.length - 1) - i : i;
            }
        }
    }

    return -1;
}

const exec = async (op) => {
    console.log(i, op, ptr, cls);

    switch (op) {
    case '[':
        if (!cls[ptr]) {
            return i = find(instr.slice(i), '[', ']', false);
        }
        break;
    case ']':
        if (cls[ptr]) {
            return i = find(instr.slice(0, i + 1), ']', '[', true);
        } else {
            ptr++;
        }
        break;
    case '+':
        !isNaN(cls[ptr]) ? cls[ptr]++ : cls[ptr] = 1;
        break;
    case '-':
        !isNaN(cls[ptr]) ? cls[ptr]-- : cls[ptr] = -1;
        break;
    case '>':
        ptr++;
        break;
    case '<':
        ptr -= ptr <= 0 ? 0 : 1;
        break;
    case ',':
        cls[ptr] = (await getChar()).charCodeAt(0);
        break;
    case '.':
        putChar(String.fromCharCode(cls[ptr]));
        break;
    default:
        break;
    }

    i++;
};

const bf = exports.bf = exports.default = (instr) => {
    let i = 0;

    if (typeof instr === 'string') {        
        brainfuck(instr);
    } else if (typeof instr === 'object' && typeof instr.on === 'function') {
        instr.on('data', (chunk) => {
            brainfuck(chunk.trim());
        });
    }

    const brainfuck = (instr) => {
        let i = 0;

        while (i < instr.length) {
            exec(instr[i]).catch(console.error);
        }
    }
};
