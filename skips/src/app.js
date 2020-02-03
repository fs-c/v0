console.log(`alpha v0.0.1`);

const neatCsv = require('neat-csv');
const csvInput = document.getElementById('csv-input');

const readFile = (file) => new Promise((resolve, reject) => {
    const reader = new FileReader();

    reader.addEventListener('load', () => {
        resolve(reader.result);
    });

    reader.addEventListener('error', () => {
        console.error('failed reading file', file);

        reject(reader.error);
    });

    reader.addEventListener('progress', ({ loaded }) => {
        console.log(`${loaded} bytes transferred`);
    });

    reader.readAsText(file, 'UTF-8');
});

const onFileSelected = async ({ target }) => {
    if (target.files.length > 1) {
        console.log('multiple files selected, ignoring all but first');
    }

    const file = target.files[0];
    const raw = await readFile(file);
    const csv = await neatCsv(raw, {
        separator: '\t',
        headers: [ 'last', 'first', 'id', 'class', 'bdate', 'btime', 'edate', 'etime', 'int', 'reason',	'textreason', 'excnr', 'status', 'exctext', 'selfreported' ],
    });

    console.log(csv);
};

csvInput.addEventListener('change', onFileSelected);
