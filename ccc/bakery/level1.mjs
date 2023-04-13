import { readFile } from 'fs/promises';

const file = await readFile('level1.in', { encoding: 'utf-8' });
const cases = file.split('\n').filter((l) => l)
    .map((l) => l.split(' ').filter((e) => e).reduce((acc, cur, i, arr) => {
        if (!(i % 3)) {
            const dest = arr[i] === 'F' ? acc.income : acc.deposits;
            const day = parseInt(arr[i + 1]);
            const value = parseInt(arr[i + 2]);

            dest[day] = value;
        }

        return acc;
    }, { income: [], deposits: [] }));

for (const c of cases) {
    for (const i in c.income) {
        if (!c.income) {
            continue;
        }

        if (c.income[i] !== c.deposits[i]) {
            console.log(i);
        }
    }

    console.log();
}
