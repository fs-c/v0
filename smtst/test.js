// const { serial, concurrent, execute } = require('./smtst');

// serial.push({func: afunc, args: []}, {func: bfunc, args: []});

// execute();

const test = require('./smtst');
const { execute } = test;

test.context = this;

const afunc = async () => {
    return 1;
};

const bfunc = async () => {
    throw new Error('Something went wrong');
};

(async () => {
    await execute.serially(afunc);
    await execute.serially(bfunc);
})();

