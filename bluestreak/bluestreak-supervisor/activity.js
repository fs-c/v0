const get = require('./get')

const INACTIVITY = 7 * 24 * 60 * 60 * 1000

const filter = (comments, max) => {
  let filtered = []

  for (let comment of comments) {
    let date = new Date(comment.date).getTime()
    if (date + max >= Date.now()) filtered.push(comment)
  }

  console.log(`filtered comments.`)

  return filtered
}

get.comments().then(comments => {
  get.members('projectbluestreak').then(members => {
    const active = [...new Set(filter(comments, INACTIVITY).map(e => e.author.name))]

    console.log(``)

    for (let member of members) {
      if (!active.includes(member.name))
        console.log(`${member.name} - steamcommunity.com/${member.customURL ? `id/${member.customURL}` : `profiles/${member.steamID}`}`)
    }
  }).catch(err => console.log(`something went wrong: ${err}`))
})
