/**
*   TODO:
*     - Largest prime factor of x. e.g.: 5 * 7 * 13 * 29 = 13195 - solution: 29
*     - What is the smallest positive number that is evenly divisible by all of the numbers from x to y?
*/

// Determines if string a is an anagram of string b.
const anagram = ([...a], [...b]) => a.sort() == b.sort() + ''

// Determines if the string a is a palindrome.
const palindrome = ([...a]) => a == a.reverse() + ''

// Determines if integer x is even or odd.
const even = x => !(x % 2)
const odd = x => x % 2 !== 0 // Could also do !!(x % 2)

// Reverses the string a.
const reverse = ([...a]) => a.map((e, i, o) => o[o.length - i - 1]).join('')
const reverse_simple = ([...a]) => a.reverse().join('')

// Outputs the solved fizzbuzz test.
const fizzbuzz = (a = []) => {
  for (let i = 1; i++ < 100;) a.push(i)
  return a.map((e, i) => !(i % 3) ? !(i % 5) ? 'FizzBuzz' : 'Fizz' : !(i % 5) ? 'Buzz' : i)
}

// Returns the factorial of integer x.
const factorial = x => x === 1 ? 1 : x * factorial(x - 1)

// Returns the greatest common factor of integers x and y.
const gcf = (x, y) => !(x % y) ? y : gcf(y, x % y)

// Returns the fibonnaci numbers up to sequence m, which is an integer.
const fibonacci = (x = 10, a = [0, 1]) => {
  console.log(x, a)
  if (a.length >= x) return a
  a.push((a.length - 1) + (a.length - 2))
  console.log((a.length - 1) + (a.length - 2))
  fibonacci(x, a)
}

const fibonacci_oneline = (m = 10, i = 1, s = '0,1', a = s.split(',').map(e => parseInt(e))) => i < m ? fibonacci_oneline (m, ++i, `${s},${a[a.length - 1] + a[a.length - 2]}`) : a
