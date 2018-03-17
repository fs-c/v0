const rl = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout,
  completer: (line, callback) => {
    const hits = [ 'one', 'two', 'three' ].filter(c => c.startsWith(line))
    callback(null, [ hits.length ? hits : [  ], line ])
  }
})

rl.on('line', input => {
  console.log(`Received ${input}`)
})