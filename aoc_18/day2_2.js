const assert = require('assert');

const rawInput = require('./utils').readInput(2);
const rawExample = require('./utils').readExample(2);

const getCommon = (str1, str2) => {
    let comm = '';

    for (let i = 0, l = str1.length; i < l; i++)
        if (str1[i] === str2[i])
            comm += str1[i];

    return comm;
};

const solve = (input) => {
    const lines = input.split('\n');

    for (const line1 of lines)
        for (const line2 of lines) {
            const comm = getCommon(line1, line2)

            if (comm.length === line1.length - 1)
                return comm;
        }
};

console.log(solve(rawInput));
