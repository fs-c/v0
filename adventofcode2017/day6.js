let banks = require('fs').readFileSync('./day6.txt', 'UTF8')
  .split('\t')
  .map(e => parseInt(e, 10))

const redist = banks => {
  let h = Math.max.apply(null, banks)
  let hi = banks.indexOf(h)
  
  banks[hi] = 0

  while (h > 0) {
    banks[++hi % banks.length]++
    h--
  }

  return banks
}

let i = 0
let seen = [  ]

while (true) {
  banks = redist(banks)
  seen.push(banks.join()) // Gotta do join() because JS is fucking retarded.

  i++
  if (seen.indexOf(banks.join()) !== seen.lastIndexOf(banks.join())) {
    console.log('P2: ' + seen.lastIndexOf(banks.join()) - seen.indexOf(banks.join()))
    break
  }
}

console.log('P1: ' + i)