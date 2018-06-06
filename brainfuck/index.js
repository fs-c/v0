const { getChar, putChar } = require('./io');

const instr = '+ + + + +';

// Position in the instructions.
let i = 0;

// "Pointer" to current active cell.
let ptr = 0;
// Cells.
const cls = [];

const findBrace = (source, from) => {
    const sub = source.slice(from);
    
    let i = 0, ign = 0, c
    for (; i <= sub.length; c = sub[i++]) {
        if (c === '[')
            ign++;
        else if (c === ']')
            if (--ign < 0) return i - 1;
    }
}

const exec = async (op) => {
    switch (op) {
    case '[':
        if (cls[ptr]) {
            ptr++;
        } else {

        }
        break;
    case ']':
        if (cls[ptr]) {
            
        } else {
            ptr++;
        }
        break;
    }
}

(async () => {

while (i++ < instr.length) {
    await exec(instr[i]);
}

})().catch(console.error);