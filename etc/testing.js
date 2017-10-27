const b64 = require('js-base64')

const permutator = (inputArr) => {
  let result = [];

  const permute = (arr, m = []) => {
    if (arr.length === 0) {
      result.push(m)
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permute(curr.slice(), m.concat(next))
     }
   }
  }

 permute(inputArr)

 return result;
}

for (let p of permutator('GTImVUWip2ImVUAioaDtpz91M2Im'.split(''))) {
  console.log(`${p} / ${b64.decode(p.join(''))}`)
}
