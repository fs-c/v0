const assert = require('assert');

const rawInput = require('./utils').readInput(2);

const parseLine = (line) => {
    const occ = [];

    for (const char of line) {
        const index = char.charCodeAt(0) - 97;

        occ[index] = occ[index] ? ++occ[index] : 1;
    }

    let two = 0;
    let three = 0;

    for (const o of occ) {
        if (o === 2)
            two = 1;
        else if (o === 3)
            three = 1;
    }

    return [ two, three ];
};

const solve = (input) => {
    let two = 0;
    let three = 0;

    for (const line of input.split('\n')) {
        let [ ntwo, nthree ] = parseLine(line);

        two += ntwo;
        three += nthree;
    }

    return two * three;
};

console.log(solve(rawInput));
