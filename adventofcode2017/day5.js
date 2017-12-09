const jumps = require('fs').readFileSync('./day5.txt', 'UTF8')
  .split('\n')
  .map(e => parseInt(e, 10))

const escape = jumps => {
  let i = -1
  let pos = 0

  while (pos >= 0 && pos <= jumps.length) {
    i++
    pos += jumps[pos]++
  }

  return i
}

console.log(escape(jumps))