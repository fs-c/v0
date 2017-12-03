/*

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

My brain hurts.

*/

const rls = require('readline-sync')

const getDist = data => {
  if (data < 1) return
  
  let counter = 1
  let corner = counter * counter

  while (corner < data) {
    counter += 2
    corner = counter * counter
  }

  let dist = Math.abs((((corner - data) % (counter - 1)) - ((counter - 1) / 2)))

  return dist + ((counter + 1) / 2) - 1
}

while (true)
  console.log(getDist(parseInt(rls.question('Input: '))))