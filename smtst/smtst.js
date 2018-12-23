exports.context = this;

exports.serial = [];
exports.concurrent = [];

const defaultPadding = 18;

exports.report = (err, name) => {
    const padded = name.padEnd(defaultPadding);
    const status = err ? err.message : 'Passed';

    console.log(`${padded}${status}`);
}

exports.execute = () => {
    (async () => {
    
    for (const task of exports.serial) {
        try {
            await (task.func.call(exports.context, ...task.args));

            exports.report(null, task.func.name);
        } catch (err) {
            exports.report(err, task.func.name);
        }
    }

    })().catch(console.error);

    for (const task of exports.concurrent) {
        task.func.call(exports.context, ...task.args).then(() => {
            exports.report(null, task.func.name);
        }).catch((err) => {
            exports.report(err, task.func.name);
        })
    }
};
