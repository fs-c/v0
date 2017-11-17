/**
* @param {integer} n - Number to be factorised.
* @return {array} - Array of prime factors.
*/
const primeFactors = (n, res = [], r = Math.sqrt(n)) => {
  let x = 2
  if (n % x) { x = 3; while ((n % x) && (x += 2) < r) {  } }

  x = (x <= r) ? x : n
  res.push(x)

  return (x === n) ? res : primeFactors(n / x, res)
}

/**
* @param {integer} x - First greatest common factor component.
* @param {integer} y - Second gcf component.
* @return {integer} - The greatest common factor of x and y.
*/
const gcf = (x, y, a = primeFactors(x), b = primeFactors(y)) =>
  (a.length > b.length ? a : b).reverse().map((e, i, c) => (c === a ? b : a).includes(e) ? e : null)[0]

console.log(gcf(30, 20))
