const { getChar, putChar } = require('./io');

const instr = ', [ ->+< ] .';

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
        if (cls[ptr]) {
            // ptr++; TODO: Is this supposed to advance the pointer?
        } else {
            const ind = find(instr.slice(i), '[', ']', false);
            i = ind;
            return;
        }
        break;
    case ']':
        if (cls[ptr]) {
            const ind = find(instr.slice(0, i + 1), ']', '[', true);
            i = ind;
            return;
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

(async () => {

while (i < instr.length) {
    await exec(instr[i]);
}

})().catch(console.error);