const { serial, concurrent, execute } = require('./smtst');

const afunc = async () => {
    return 1;
};

const bfunc = async () => {
    throw new Error('Something went wrong');
};

serial.push({func: afunc, args: []}, {func: bfunc, args: []});

execute();
