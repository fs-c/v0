const Community = require('steamcommunity')
const community = new Community()

const INACTIVITY = 7 * 24 * 60 * 60 * 1000

const getMembers = group => {
  return new Promise((resolve, reject) => {
    community.getSteamGroup(group, (err, group) => {
      if (err) return reject(err)
      console.log(`got steam group.`)

      group.getMembers((err, members) => {
        if (err) return reject(err)
        console.log(`got group members.`)

        let p = members.map(m => getUser(m))

        Promise.all(p)
        .then(users => {
          console.log(`got all user profiles.`)
          resolve(users)
        }).catch(err => reject(err))
      })
    })
  })
}

const getUser = id => {
  return new Promise((resolve, reject) => {
    community.getSteamUser(id, (err, user) => {
      if (err) return reject(err)

      resolve(user)
    })
  })
}

const filter = (comments, max) => {
  let filtered = []

  for (let comment of comments) {
    let date = new Date(comment.date).getTime()
    if (date + max >= Date.now()) filtered.push(comment)
  }

  console.log(`filtered comments.`)

  return filtered
}

require('./get').comments().then(comments => {
  getMembers('projectbluestreak').then(members => {
    const active = [...new Set(filter(comments, INACTIVITY).map(e => e.author.name))]

    console.log(``)

    for (let member of members) {
      if (!active.includes(member.name))
        console.log(`${member.name} - steamcommunity.com/${member.customURL ? `id/${member.customURL}` : `profiles/${member.steamID}`}`)
    }
  }).catch(err => console.log(`something went wrong: ${err}`))
})
