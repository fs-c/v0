import path from 'path';
import logger from 'debug';
import fs from 'fs/promises';
import ExcelJS from 'exceljs';

const debug = logger('nabevert:lib');

export const getOverviewFiles = async (fromPath) => {
    const directories = (await fs.readdir(fromPath, { withFileTypes: true }))
        .filter((e) => e.isDirectory())
        .map((e) => e.name)
        .filter((n) => /^-?\d+$/.test(n.split(' ')[0])
            && !n.toLowerCase().includes('schluss'));

    const findOverviewFile = async (directory) => {
        const dirPath = path.join(fromPath, directory);
        const files = (await fs.readdir(dirPath, { withFileTypes: true }));

        for (const file of files) {
            if (!file.name.toLowerCase().includes('übersicht') ||
                file.name.includes('~$')
            ) {
                continue;
            }

            if (file.isDirectory()) {
                return findOverviewFile(path.join(directory, file.name));
            }

            if (file.isFile()) {
                return path.join(dirPath, file.name);
            }
        }
    };

    const overviewFiles = (await Promise.all(directories.map(findOverviewFile)))
        .filter((e) => e);
        
    return overviewFiles;
};

export const getCoordinator = async (overviewFile) => {
    debug('getting coordinator for %o', overviewFile);

    const workbook = new ExcelJS.Workbook();
    await workbook.xlsx.readFile(overviewFile);

    const worksheet = workbook.getWorksheet('Gesamtübersicht PM NABE')
        || workbook.getWorksheet(`Gesamtübersicht ${new Date().getFullYear()}`)
        || workbook.getWorksheet(`Gesamtübersicht ${new Date().getFullYear()}${(new Date().getFullYear() + 1 + '').slice(2)}`);

    if (!worksheet) {
        debug('could not find worksheet');

        return;
    }

    // Get the first 20 rows, this is an arbitrary number
    const totalRows = 20;
    const rows = worksheet.getRows(1, totalRows);

    for (let rowIdx = 0; rowIdx < totalRows; rowIdx++) {
        const row = rows[rowIdx];

        // Iterate over the first 9 columns, again an arbitrary number
        for (let colIdx = 1; colIdx < 10; colIdx++) {
            const cell = row.getCell(colIdx);

            if (!cell.fill || !cell.fill.fgColor || cell.fill.fgColor.argb !== 'FF66FFFF') {
                continue;
            }

            debug('coordinator row at %o (found at col %o)', rowIdx, colIdx);

            const values = row.values;

            if (values[4] && values[4].toLowerCase().includes('koordinator')) {
                return;
            }

            return { name: values[4] + ' ' + values[5],
                mail: values[7] ? values[7].text || values[7] : null, phone: values[8] };
        }
    }
};
