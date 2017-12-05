const phrases = require('fs').readFileSync('./day4.txt', 'UTF8')
  .split('\n')
  .map(e => e.split(' ')
    .map(e => e.trim()))

const p1_valid = phrase => 
  [ ...new Set(phrase) ].length === phrase.length

const p2_valid = phrase => {
  for (let i = 0; i < phrase.length; i++)
    for (let j = 0; j < phrase.length; j++)
      if (j !== i && [ ...phrase[i] ].sort() == [ ...phrase[j] ].sort() + '')
        return false

  return true
}

console.log('P1: ' + phrases.reduce((a, c) => a += p1_valid(c) ? 1 : 0, 0))
console.log('P2: ' + phrases.reduce((a, c) => a+= p2_valid(c) ? 1 : 0, 0))
