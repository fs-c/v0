const options = {delay: 2000, other: 'value'}

const final = Object.assign({
  delay: 1000
}, options);

console.log(final);