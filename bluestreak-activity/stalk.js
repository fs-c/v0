const comments = require('./comments')

const rls = require('readline-sync')
const moment = require('moment')

const users = findUsers(rls.question(`User: `))

let user = users[rls.keyInSelect(users, `Which user?`)]

function findUsers (input) {
  input = input.toLowerCase()

  let p = []
  for (let cmt of comments) {
    let a = cmt.author
    if (
      a.name.toLowerCase().indexOf(input) !== -1 ||
      // (a.customURL && a.customURL.toLowerCase().indexOf(input) !== -1) ||
      (a.id && a.id.indexOf(input) !== -1)
    ) if (!p.includes(a.name)) p.push(a.name)
  }
  return p
}

user = fetchData(user)

function fetchData (name) {
  let oldest = Infinity
  let length = 0
  let total = 0
  let time = {}

  for (let cmt of comments) {
    if (cmt.author.name === name) {
      total++
      length += cmt.text.length

      let date = new Date(cmt.date)

      if (!oldest) oldest = date

      if (date < oldest) oldest = date

      let hours = date.getHours()
      if (time[hours]) { time[hours]++ } else time[hours] = 1
    }
  }

  return { total, length, time, oldest }
}

console.log(``)
console.log(`The user has posted a total of ${user.total} comments, with an average length of ${Math.round(user.length / user.total)} characters.`)
console.log(`The first recorded comment of this user was ${moment(user.oldest).fromNow()}.`)
console.log(``)
