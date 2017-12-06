/*

Moves are always exactly one house to the north (^), south (v), east (>), or west (<). 
After each move, he delivers another present to the house at his new location.

Part 2:
  Santa and Robo-Santa start at the same location (delivering two presents 
  to the same starting house), then take turns moving.

*/

const moves = require('fs').readFileSync('./day3.txt', 'UTF8').split('')

const simulate = moves => {
  let i = 0

  const pos = [ [ 0, 0 ], [ 0, 0 ] ]

  let visited = [ pos[0].join('/'), pos[1].join('/') ]

  for (const move of moves) {
    i++

    switch (move) {
      case '^': pos[i % 2][0]++
        break
      case 'v': pos[i % 2][0]--
        break
      case '>': pos[i % 2][1]++
        break
      case '<': pos[i % 2][1]--
        break
    }

    visited.push(pos[i % 2].join('/'))
  }

  let unique = [ ...new Set(visited) ]

  return unique.length
}

console.log(simulate(moves))