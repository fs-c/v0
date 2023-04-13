import { readFile } from 'fs/promises';

const file = await readFile('./level3.in', { encoding: 'utf-8' });
const cases = file.split('\n').filter((l) => l)
    .map((l) => l.split(' ').filter((e) => e).reduce((acc, cur, i, arr) => {
        if (!(i % 3)) {
            const dest = arr[i] === 'F' ? acc.income : acc.deposits;
            const day = parseInt(arr[i + 1]);
            const value = parseInt(arr[i + 2]);

            dest.push({ day, value });
        }

        return acc;
    }, { income: [], deposits: [] }));

const checkDeposits = (entry, deposits) => {
    const maxDepth = 4;
    
    const rec = (sum, chain) => {
        if (sum === entry.value) {
            return true;
        }

        if (chain.length > maxDepth) {
            return false;
        }

        for (const d of deposits) {
            if (d.day < entry.day) {
                continue;
            }

            if (chain.includes(d)) {
                continue;
            }

            if (rec(sum + d.value, [ ...chain, d ])) {
                d.value = 0;

                return true;
            }
        }

        return false;
    };

    return rec(0, []);
};

for (const c of cases) {
    const invalidDays = [];

    for (const entry of c.income) {
        if (!checkDeposits(entry, c.deposits)) {
            invalidDays.push(entry.day);
        }
    }

    console.log(invalidDays.join(' '));
}
