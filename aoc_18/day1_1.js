const assert = require('assert');

const rawInput = require('./utils').readInput(1);

const solve = (deltas) => {
    return deltas.reduce((cur, acc) => acc += cur, 0);
}

assert(solve([ 1, 1, 1 ]) === 3);
assert(solve([ 1, 1, -2 ]) === 0);
assert(solve([ -1, -2, -3 ]) === -6);

console.log(solve(rawInput));
