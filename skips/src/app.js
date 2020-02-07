console.log(`alpha v0.0.1`);

const { parseCsv } = require('./csv');
const { parseIcs } = require('./ics');

let currentAbsences = null;
let currentTimetable = null;

const csvInput = document.getElementById('csv-input');
const icsInput = document.getElementById('ics-input');
const generateButton = document.getElementById('generate-button');

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

const onFileSelected = (f) => async ({ target }) => {
    if (target.files.length > 1) {
        console.warn('multiple files selected, ignoring all but first');
    }

    const file = target.files[0];

    if (!file) {
        throw new Error('could not get file');
    }

    const raw = await readFile(file);

    if (!raw) {
        throw new Error('could not read file');
    }

    await f(raw);

    if (currentAbsences && currentTimetable) {
        generateButton.disabled = false;
    }
};

const onCsvSelected = async (raw) => {
    currentAbsences = await parseCsv(raw);

    console.log({ currentAbsences });
};

const onIcsSelected = async (raw) => {
    currentTimetable = await parseIcs(raw);

    console.log({ currentTimetable });
};

const onGenerateClick = async () => {
};

csvInput.addEventListener('input', onFileSelected(onCsvSelected));
icsInput.addEventListener('input', onFileSelected(onIcsSelected));

generateButton.addEventListener('click', onGenerateClick);
