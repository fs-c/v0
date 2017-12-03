/*

A list of dimensions (length, width, height) is provided.

Part 1:
  Find the surface area of the box + the area of the smallest side.

Part 2:
  Shortest distance around its sides, or the smallest perimeter of any one face 
  + the cubic feet of volume of the box.

*/


const fs = require('fs')

// [ [ l, w, h ], [ ... ], ... ]
const parse = t => t.split('\n').map(box => box.split('x').map(e => parseInt(e.trim())))

const dimensions = parse(fs.readFileSync('./day2.txt', 'UTF8'))

const getPaper = d => {
  let s1 = 2 * d[0] * d[1], s2 = 2 * d[1] * d[2], s3 = 2 * d[2] * d[0]
 
  return s1 + s2 + s3 + (Math.min(s1, s2, s3) / 2)
}

console.assert(getPaper([ 2, 3, 4 ]) === 58)

const getTotalPaper = boxes => boxes.reduce((acc, c) => {
  let p = getPaper(c)
  return acc += (p && p !== NaN) ? p : 0
}, 0)

const getRibbon = d =>
  (d.sort((a, b) => a - b).slice(0, 2).reduce((a, c) => a + c) * 2) + 
  d.reduce((a, c) => a * c)

console.assert(getRibbon([ 2, 3, 4 ]) === 34)
console.assert(getRibbon([ 1, 1, 10 ]) === 14)

const getTotalRibbon = boxes => boxes.reduce((acc, c) => {
  let p = getRibbon(c)
  console.log(acc, p)
  return acc += (p && p !== NaN) ? p : 0
}, 0)

console.log(getTotalRibbon(dimensions))