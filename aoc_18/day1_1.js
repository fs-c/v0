const assert = require('assert');

const input = require('fs').readFileSync('day1_input.txt', 'utf8').split('\n')
    .map((e) => parseInt(e, 10));

const solve = (deltas) => {
    return deltas.reduce((cur, acc) => acc += cur, 0);
}

assert(solve([ 1, 1, 1 ]) === 3);
assert(solve([ 1, 1, -2 ]) === 0);
assert(solve([ -1, -2, -3 ]) === -6);

console.log(solve(input));
