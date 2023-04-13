import { readFile } from 'fs/promises';

const file = await readFile('./level5.in', { encoding: 'utf-8' });
const cases = file.split('\n').filter((l) => l)
    .map((l) => l.split(' ').filter((e) => e).reduce((acc, cur, i, arr) => {
        if (cur === 'F') {
            acc.income.push({
                day: parseInt(arr[i + 1]),
                driver: parseInt(arr[i + 2]),
                timeFrame: parseInt(arr[i + 3]),
                value: parseInt(arr[i + 4]),
            });
        } else if (cur === 'B') {
            acc.deposits.push({
                day: parseInt(arr[i + 1]),
                value: parseInt(arr[i + 2]),
            });
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
            if (d.day < entry.day || (d.day - entry.day) > entry.timeFrame) {
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
    const invalidEntries = [];

    for (const entry of c.income) {
        if (!checkDeposits(entry, c.deposits)) {
            invalidEntries.push(entry);
        }
    }

    console.log(invalidEntries.sort((a, b) => (a.day - b.day) || (a.driver - b.driver))
        .map((e) => e.day + ':' + e.driver).join(' '));
}
