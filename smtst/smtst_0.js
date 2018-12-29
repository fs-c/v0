exports.serial = [];
exports.concurrent = [];

const defaultPadding = 18;

exports.test = (func, ...args) => {
    exports.concurrent.push({ func, args });
};

exports.test.serial = (func, ...args) => {
    exports.serial.push({ func, args });
};

const pass = (string) => `\x1b[1m\x1b[32m${string}\x1b[0m`;
const fail = (string) => `\x1b[1m\x1b[31m${string}\x1b[0m`;
const time = (string) => `\x1b[1m\x1b[90m${string}\x1b[0m`;

let last = 0;

exports.passed = (name) => {
    console.log(`${pass(name.padEnd(defaultPadding))} succeeded`
        + ` ${time('+' + (Date.now() - last) + 'ms')}`);

    last = Date.now();
};

exports.failed = (name, error = {}) => {
    console.error(`${fail(name.padEnd(defaultPadding))} failed`
    + ` ${time(Date.now() - last + 'ms')}`);

    if (error.message) {
        console.error(`${''.padEnd(defaultPadding)} ${error.message}`);
    }

    last = Date.now();
};

exports.execute = () => {
    last = Date.now();

    (async () => {

    for (const task of exports.serial) {
        try {
            await task.func(...task.args);

            exports.passed(task.func.name);
        } catch (err) {
            exports.failed(task.func.name, err);
        }
    }

    })().catch(console.error);

    for (const task of exports.concurrent) {
        task.func(...task.args).then(() => {
            exports.passed(task.func.name);
        }).catch((err) => {
            exports.failed(task.func.name, err);
        })
    }
};
