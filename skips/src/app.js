console.log(`alpha v0.1.0`);

const TOTAL_WEEKS = 40;
const SCHOOL_START = new Date(2019, 8, 9);

const { parseCsv } = require('./csv');
const { parseIcs } = require('./ics');
const { readFile, daysIntoYear } = require('./utils');

let currentAbsences = null;
let currentTimetable = null;

const csvInput = document.getElementById('csv-input');
const icsInput = document.getElementById('ics-input');
const resultBox = document.getElementById('result-box');
const generateButton = document.getElementById('generate-button');

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
    const subjects = {};
    const absences = {};

    for (const lesson of currentTimetable.flat()) {
        subjects[lesson.subject] = (subjects[lesson.subject] || 0) + 1;
    }

    for (const absence of currentAbsences.absences) {
        const lessons = currentTimetable[absence.day].filter((l) =>
            l.begin.hour >= absence.begin.hour && l.end.hour <= absence.end.hour
        );

        if (!lessons.length) {
            console.warn('no applicable lessons found for absence', absence);

            continue;
        }

        for (const { subject } of lessons) {
            absences[subject] = (absences[subject] || 0) + 1;
        }
    }

    console.log({ subjects, absences });
};

csvInput.addEventListener('input', onFileSelected(onCsvSelected));
icsInput.addEventListener('input', onFileSelected(onIcsSelected));

generateButton.addEventListener('click', onGenerateClick);
