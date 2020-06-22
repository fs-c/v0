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

    reader.readAsArrayBuffer(file);
});

exports.readFile = readFile;

const onColorSchemeChange = (callback) => {
    const dark = matchMedia('(prefers-color-scheme: dark)');

    dark.addEventListener('change', (e) => {
        if (e.matches) {
            callback('dark');
        } else {
            // TODO: For all we know this could be 'red' or something. DiD yOu 
            // juSt AsSumE mY ColOr sCheME?
            callback('light');
        }
    });
};

exports.onColorSchemeChange = onColorSchemeChange;

const equalArray = (arr1, arr2) => {
    const a1len = arr1.length;

    for (let i = 0; i < a1len; i++)
        if (arr1[i] !== arr2[i])
            return false;

    return true;
}

exports.equalArray = equalArray;

const onFileAdded = (f) => async ({ target }) => {
    if (target.files.length > 1) {
        console.warn('multiple files selected, ignoring all but the first');
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
};

exports.onFileAdded = onFileAdded;

const getColorScheme = () => ({
    background: '255,255,255',
    foreground: '0,0,0',
    highlight: [
        '0,141,213',
        '255,99,132',
    ],
});

exports.getColorScheme = getColorScheme;
