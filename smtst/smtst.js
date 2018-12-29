exports.padding = 18;
exports.context = this;

exports.report = (err, name) => {
    const padded = name.padEnd(exports.padding);
    const status = err ? err.data.message : 'Passed';

    console.log(`${padded}${status}`);
}

exports.execute = (func, args = []) => {
    const bound = func.bind(exports.context, ...args);

    bound().then(() => {
        exports.report(null, func.name);
    }).catch((err) => {
        exports.report(err, func.name);
    });
}

exports.execute.serially = async (func, args = []) => {
    const bound = func.bind(exports.context, ...args);

    let err = null;
    try {
        await bound();
    } catch (catched) {
        err = catched;
    }

    exports.report(err, func.name);
}
