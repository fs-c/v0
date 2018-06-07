const { getChar, putChar } = require('./io');

const instr = ', [ ->+< ] .';

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

    let i = closing ? 0 : sub.length - 1, ign = 0, c;
    for (; closing ? i <= sub.length : i >= 0; c = sub[closing ? i++ : i--]) {
        if (c === closing ? '[' : ']')
            ign++;
        else if (c === closing ? ']' : '[')
            if (--ign < 0) return i + closing ? -1 : 1;
    }

    return -1;
};

const exec = async (op) => {
    console.log(`exc: '${op}' - ${i} - ${cls} - ${ptr}`);

    switch (op) {
    case '[':
        if (cls[ptr]) {
            ptr++;
        } else {
            const ind = findBrace(instr, i, true);
            console.log('closing', ind);
            i = ind;
            return;
        }
        break;
    case ']':
        if (cls[ptr]) {
            const ind = findBrace(instr, i, false);
            console.log('opening: ', ind);
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