let total = 0;
const runs = process.argv[2];

for (let i = 0; i < runs; i++)
    total += Math.round(Math.random());

console.log(total / runs);
