const data = require('./comments')

const babar = require('babar')

console.log(`Parsing ${data.length} comments.`)
console.log(``)
console.log(`Top 10 members by comments posted: `)

function valid (name) {
  name = name.toLowerCase()
  if (name.indexOf('[bluestreak]') !== -1 || name.indexOf('[bls]') !== -1) return true
  return false
}

let total = 0
let count = {}
let length = {}
let time = {}
let days = {}
let rank = {}
for (let i in data) {
  let cmt = data[i]

  if (!valid(cmt.author.name)) continue

  total += cmt.text.length

  if (rank[cmt.author.rank]) {
    rank[cmt.author.rank].count++
    rank[cmt.author.rank].length += cmt.text.length
  } else rank[cmt.author.rank] = { count: 1, length: cmt.text.length }

  let hour = new Date(cmt.date).getHours()
  if (time[hour]) time[hour]++
  else time[hour] = 1

  let day = new Date(cmt.date).getDay()
  if (days[day]) days[day]++
  else days[day] = 1

  let id = cmt.author.customURL || cmt.author.id
  if (count[id]) count[id].num++
  else count[id] = { num: 1, name: cmt.author.name }

  if (length[id]) length[id].length += cmt.text.length
  else length[id] = { length: cmt.text.length, name: cmt.author.name }
}

let top = []
for (let i of Object.keys(count)) top.push(count[i])
top.sort((a, b) => a.num > b.num ? -1 : 1)

// Eww.
for (let i = 0; i++ < 10;) console.log(`${i.toString().padEnd(5)} ${top[i - 1].num} comments, ${top[i - 1].name}`)

console.log(``)

let avg = []
for (let i of Object.keys(length)) avg.push({ length: Math.ceil(length[i].length / count[i].num), name: length[i].name })
avg.sort((a, b) => a.length > b.length ? -1 : 1)

for (let i = 0; i++ < 10;) console.log(`${avg[i - 1].length.toString().padEnd(5)} average characters ${avg[i - 1].name}`)

console.log(``)
console.log(`The average comment length is ${Math.ceil(total / data.length)} characters.`)
console.log(``)

console.log(`Regular members make ${Math.round((rank['Member'].count / data.length) * 100)}%, moderators ${Math.round((rank['Moderator'].count / data.length) * 100)}% and officers ${Math.round((rank['Officer'].count / data.length) * 100)}% of the comments.`)
console.log(`Note that values are rounded.`)

console.log(``)

let timeChart = []
for (let i of Object.keys(time)) timeChart.push([ i, time[i] ])

console.log(babar(timeChart, {
  yFractions: 0,
  caption: 'Total number of comments/hour (UTC).'
}))

let daysChart = []
for (let i of Object.keys(days)) daysChart.push([ i, days[i] ])

console.log(``)
console.log(babar(daysChart, {
  caption: 'Total number of comments/day of the week. (Mon - Sun)'
}))
