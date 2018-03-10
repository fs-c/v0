const rls = require('readline-sync');

const build = (s, e = 7) => {
  for (let i = 1; i < (e * 2); i++)
    console.log(s *= i <= e ? i : 1 / (((e * 2) - i) + 1)); }

build(rls.question('Start: '));