class MarkovChain {
  constructor (contents) {
    this.bank = {  }

    this.parse(contents)
  }

  parse (contents) {
    const words = contents.split(' ').filter(w => w.trim() !== '')

    for (let i = 0; i < words.length - 1; i++) {
      const word = words[i], next = words[i + 1]

      if (!this.bank[word]) this.bank[word] = {  }
      if (!this.bank[word][next]) {
        this.bank[word][next] = 1
      } else this.bank[word][next] += 1
    }

    return this
  }

  process (start, length) {
    let sentence = start

    for (let i = 0; i < length; i++) {
      let word = pickOneByWeight(this.bank[start])
      sentence += ' ' + word
      start = word
    }

    return sentence
  }
}

function pickOneByWeight (obj) {
  const keys = Object.keys(obj)
  const sum = keys.reduce((p, c) => p + obj[c], 0)
  const choose = ~~(Math.random() * sum)

  let count = 0
  for (const key of keys) {
    count += obj[key]
    if (count > choose) return key
  }
}


module.exports = MarkovChain
