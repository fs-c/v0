const convert = require('./convert')

convert('./image.jpg').then(console.log).catch(console.error)
