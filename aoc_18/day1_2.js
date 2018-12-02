const assert = require('assert');

const rawInput = require('fs').readFileSync('day1_input.txt', 'utf8');

const insert = (arr, val) => {
    let mid;
    let lo = 0;
    let hi = arr.length;

    while (lo < hi) {
        mid = (lo + hi) >>> 1;

        if (arr[mid] < val)
            lo = mid + 1;
        else hi = mid;
    }

    arr.splice(lo, 0, val);
};

const find = (arr, val) => {
    let mid;
    let lo = 0;
    let hi = arr.length;

    while (lo < hi) {
        mid = (lo + hi) >>> 1;

        if (arr[mid] === val)
            return mid;

        if (arr[mid] < val)
            lo = mid + 1;
        else hi = mid;
    }

    return -1;
};

const solve = (input, sep = '\n') => {
    const frequencies = [ ];
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
