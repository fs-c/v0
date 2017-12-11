const raw = require('fs').readFileSync('./day7.txt', 'utf8')
  .split('\n')
  .map(e => e.trim())
  .map(e => { return {
    name: e.slice(0, e.indexOf(' ')),
    weight: parseInt(e.slice(e.indexOf('(') + 1, e.indexOf(')')), 10),
    carrying: e.indexOf('->') !== -1 ? e.slice(e.indexOf('->') + 3).split(', ') : []
  }})

const getByName = (name, raw = raw) => {
  for (const e of raw)
    if (e.name === name)
      return e
}

const getBottom = raw => {
  const heaviest = raw.sort((a, b) => b.weight - a.weight)[0]

  for (const e of raw)
    if (e.carrying.includes(heaviest.name))
      return e
}