const comments = require('./comments')

// const rls = require('readline-sync')

// let user = rls.question(`Enter vanityURL name or steamID of user: `)

console.log(findUsers('ejjp'))

function findUsers (input) {
  let p = []
  for (let cmt of comments) {
    let a = cmt.author
    if (
      a.name.indexOf(input) !== -1 ||
      (a.customURL && a.customURL.indexOf(input) !== -1) ||
      (a.id && a.id.indexOf(input) !== -1)
    ) if (!p.includes(a.name)) p.push(a.name)
  }
  return p
}
