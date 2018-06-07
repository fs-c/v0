const { getChar, putChar } = require('./io');

const instr = ', + + + + + .';

// Position in the instructions.
let i = 0;

// "Pointer" to current active cell.
let ptr = 0;
// Cells.
const cls = [];

/**
 * Searches for either a matching closing or opening brace in the source string,
 * starting at from. Returns the index or -1 if none was found.
 * 
 * TODO: Make this less ugly. (lighter use of tern op, clearer for)
 */
const findBrace = (source, from, closing) => {
    const sub = source.slice(...(closing ? [ from ] : [ 0, from ]));

    let i = closing ? 0 : source.length - 1, ign = 0, c;
    for (; closing ? i <= sub.length : i >= 0; c = sub[closing ? i++ : i--]) {
        if (c === closing ? '[' : ']')
            ign++;
        else if (c === closing ? ']' : '[')
            if (--ign < 0) return i + closing ? -1 : 1;
    }

    return -1;
};

const exec = async (op) => {
    switch (op) {
    case '[':
        if (cls[ptr]) {
            ptr++;
        } else {
            i = findBrace(instr, i, true);
        }
        break;
    case ']':
        if (cls[ptr]) {
            i = findBrace(instr, i, false);
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
};

(async () => {

while (i < instr.length) {
    await exec(instr[i++]);

    console.log(i, ptr, cls);
}

})().catch(console.error);