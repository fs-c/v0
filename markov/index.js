const fs = require('fs')
const MarkovChain = require('./markov')

const chain = new MarkovChain(fs.readFileSync('data.txt', 'utf8'))

console.log(chain.process('Interactive', 5))
