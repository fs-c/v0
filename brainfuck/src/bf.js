const { getChar, putChar } = require('./io');

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
};

const exec = async (instr, i) => {
    const op = instr[i];

    switch (op) {
    case '[':
        if (!cls[ptr]) {
            // Jump forwards to the matching closing bracket.
            return i = find(instr.slice(i), '[', ']', false);
        } // Don't increment pointer if the check fails, this is disputed.
        break;
    case ']':
        if (cls[ptr]) {
            // Jump backwards to the matching opening bracked.
            return i = find(instr.slice(0, i + 1), ']', '[', true);
        } else {
            ptr++;
        }
        break;
    case '+':
        // Cells are initialized to `undefined`, set to one if unitialized.
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

    return ++i;
};

const bf = module.exports = (instr) => {
    const brainfuck = async (instr) => {
        let i = 0;

        while ((i = await exec(instr, i)) < instr.length)
            ;
    };

    if (typeof instr === 'string') {        
        brainfuck(instr);
    } else if (typeof instr === 'object' && typeof instr.on === 'function') {
        let instrstack = '';
        
        instr.on('data', (chunk) => instrstack += chunk);
        instr.on('end', () => {
            brainfuck(instrstack);
        });
    }
};
