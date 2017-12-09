const reg = {  }

const instructions = require('fs').readFileSync('./day8.txt', 'utf8')
  .split('\n')
  .map(e => {
    e = e.split(' ')

    console.log(e)

    if (!reg[e[0]])
      reg[e[0]] = 0

    e[1] = e[1] === 'inc' ? '+=' : '-='

    return `(reg.${e[4]} ${e[5]} ${parseInt(e[6].trim(), 10)}) ? ` +
      `reg.${e[0]} ${e[1]} ${parseInt(e[2], 10)} : null`
  })

for (const inst of instructions)
  eval(inst)