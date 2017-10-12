// Determines if string a is an anagram of string b.
const anagram = ([...a], [...b]) => a.sort() == b.sort() + ''

// Determines if the string a is a palindrome.
const palindrome = ([...a]) => a == a.reverse() + ''

// Determines if integer x is even or odd.
const even = x => !(x % 2)
const odd = x => x % 2 !== 0 // Could also do !!(x % 2)

// Reverses the string a.
const reverse = ([...a]) => a.map((e, i, o) => o[o.length - i - 1]).join('')

// Outputs the solved fizzbuzz test.
const fizzbuzz = (a = []) => {
  for (let i = 1; i++ < 100;) a.push(i)
  return a.map((e, i) => !(i % 3) ? !(i % 5) ? 'FizzBuzz' : 'Fizz' : !(i % 5) ? 'Buzz' : i)
}

// Returns the faculty of integer x.
const faculty = x => x === 1 ? 1 : x * faculty(x - 1)

console.log(faculty(10))
