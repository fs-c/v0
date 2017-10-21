const comments = require('./comments')

const rls = require('readline-sync')
const moment = require('moment')

let users = findUsers(rls.question(`User: `))
let user = users[rls.keyInSelect(users, `Which user?`)]

function findUsers (input) {
  input = input.toLowerCase()

  let p = []
  for (let cmt of comments) {
    let a = cmt.author
    if (
      a.name.toLowerCase().indexOf(input) !== -1 ||
      (a.customURL && a.customURL.toLowerCase().indexOf(input) !== -1) ||
      (a.id && a.id.indexOf(input) !== -1)
    ) if (!p.includes(a.name)) p.push(a.name)
  }
  return p
}

let { total, length, time } = fetchData(user)

function fetchData (name) {
  let first = Infinity
  let total = 0
  let length = 0
  let time = {}
  
  for (let cmt of comments) {
    if (cmt.author.name === name) {
      total++
      length += cmt.text.length

      let date = new Date(cmt.date)

      if (date > first) first = date
      let hours = date.getHours()
      if (time[hours]) { time[hours]++ } else time[hours] = 1
    }
  }

  return { total, length, time }
}

console.log(``)
console.log(`The user has posted a total of ${total} comments, with an average length of ${Math.round(length / total)} characters.`)
console.log(`The first recorded comment of this user was ${moment(time).fromNow()}.`)
console.log(``)
