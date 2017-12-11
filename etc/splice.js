const string = 'aaaaaaaaabccccccccdaaaabcdaaa'.split('')

let i = string.indexOf('b'), j = string.indexOf('d')

while (i !== -1 && j !== -1) {
  i = string.indexOf('b'), j = string.indexOf('d')

  string.splice(i, j - i + 1)
}

console.log(string)