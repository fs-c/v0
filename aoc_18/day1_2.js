const assert = require('assert');

const utils = require('./utils');
const { find, insert } = utils.sortedArray;

const rawInput = utils.readInput(1);

const solve = (input, sep = '\n') => {
    const frequencies = [];
    const deltas = input.split(sep).map((e) => parseInt(e, 10));

    let i = 0;
    let freq = 0;    
    while (true) {
        const delta = deltas[i %= deltas.length];

        insert(frequencies, freq);

        freq += delta;

        if (find(frequencies, freq) !== -1)
            return freq;

        i++;
    }
};

assert(solve('+1, -1', ', ') === 0);
assert(solve('+3, +3, +4, -2, -4', ', ') === 10);
assert(solve('-6, +3, +8, +5, -6', ', ') === 5);

console.log(solve(rawInput));
