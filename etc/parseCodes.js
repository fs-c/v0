const fs = require('fs');

const codes = fs.readFileSync('codes.json', 'utf8')
  .split('\n')
  .map((e) => e.trim())
  .map((e) => e.slice(e.indexOf('|') + 2))
  .forEach((e) => fs.appendFileSync('codesOut', e + '\n'))