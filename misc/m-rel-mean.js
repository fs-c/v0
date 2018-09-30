let runs = process.argv[2], total = 0;

console.time('ran in');

for (let i = 0; i < runs; i++)
    total += Math.round(Math.random());

console.log(total / runs);
console.timeEnd('ran in');
