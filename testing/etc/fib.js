/**
* @param {integer} m - The length of the fibonacci sequence.
* @return {array} - The sequence.
*/
const fib = (m, i = 1, s = '0,1', a = s.split(',').map(e => parseInt(e))) =>
  i < m ? fib(m, ++i, `${s},${a[a.length - 1] + a[a.length - 2]}`) : a

console.log(fib(100))
