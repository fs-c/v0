let stream = require('fs').readFileSync('./day9.txt', 'utf8')
  .split('')

const clean = s => {
  let r = [  ]
  let d = false

  for (let i = 0; i < s.length; i++) {
    if (s[i] === '<') d = true

    if (!d) r.push(s[i])

    if (s[i] === '>' && s[i - 1] !== '!') d = false
  }

  return r
}

const total = group => {
  let total = 0

  const iterate = (sub, level) => {
    console.log(`level ${level} - ${sub}`)

    if (!sub[0])
      console.log(`found a bottom`)

    for (const obj of sub) {
      console.log(`level ${level} substack - ${obj}`)      

      total += level

      iterate(obj, level + 1)
    }
  }

  iterate(group, 1)

  return total
}

const object = JSON.parse('"' + clean(stream).join('') + '"')

require('fs').writeFileSync('./day9_cleaned.json', object)