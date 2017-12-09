let banks = require('fs').readFileSync('./day6.txt', 'UTF8')
  .split('\t')
  .map(e => parseInt(e, 10))

const redist = banks => {
  let h = Math.max.apply(null, banks)
  let hi = banks.indexOf(h)
  
  banks[hi] = 0

  let i = hi + 1
  while (h > 0) {
    banks[++i % banks.length]++
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
  if (seen.indexOf(banks.join()) !== seen.lastIndexOf(banks.join()))
    break
}

console.log(i)