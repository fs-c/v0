const { getChar, putChar } = require('./io');

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
};

const exec = async (instr, i, ptr, cls) => {
    const op = instr[i];

    switch (op) {
    case '[':
        if (!cls[ptr]) {
            // Jump forwards to the matching closing bracket.
            const closing = find(instr.slice(i), '[', ']', false);

            return { i: closing, ptr, cls };
        } // Don't increment pointer if the check fails, this is disputed.
        break;
    case ']':
        if (cls[ptr]) {
            // Jump backwards to the matching opening bracket.
            const opening = find(instr.slice(0, i + 1), ']', '[', true);

            return { i: opening, ptr, cls };
        } else {
            ptr++;
        }
        break;
    case '+':
        // Cells are initialized to `undefined`, start at one if it's not been
        // touched before.
        !isNaN(cls[ptr]) ? cls[ptr]++ : cls[ptr] = 1;
        break;
    case '-':
        // See above.
        !isNaN(cls[ptr]) ? cls[ptr]-- : cls[ptr] = -1;
        break;
    case '>':
        ptr++;
        break;
    case '<':
        // Don't decrement beyond zero.
        ptr -= ptr <= 0 ? 0 : 1;
        break;
    case ',':
        // Block until a char is retrieved from stdin.
        cls[ptr] = (await getChar()).charCodeAt(0);
        break;
    case '.':
        // Output the corresponding character, not the number.
        putChar(String.fromCharCode(cls[ptr]));
        break;
    default:
        break;
    }

    return { i: ++i, ptr, cls };
};

const bf = module.exports = (instr, cb = () => {}) => {
    const brainfuck = async (instr) => {
        // Current index in the instructions.
        let i = 0;
        // "Pointer" to (aka index of) the active cell.
        let ptr = 0;
        // Array of cells.
        const cls = [];

        while (({ i, ptr, cells } = await exec(instr, i, ptr, cls)).i
            < instr.length);
        
        cb({ index: i, pointer: ptr, cells: cls });
    };

    if (typeof instr === 'string') {        
        brainfuck(instr);
    } else if (typeof instr === 'object' && typeof instr.on === 'function') {
        let stack = '';
        
        instr.on('data', (chunk) => stack += chunk);
        instr.on('end', () => {
            brainfuck(stack);
        });
    }
};
