const Community = require('steamcommunity')
const community = new Community()

const log = require('../logger')

const comments = require('./comments')

const INACTIVITY = 7 * 24 * 60 * 60 * 1000

const getMembers = group => {
  return new Promise((resolve, reject) => {
    community.getSteamGroup(group, (err, group) => {
      if (err) return reject(err)
      log.debug(`got steam group.`)

      group.getMembers((err, members) => {
        if (err) return reject(err)
        log.debug(`got group members.`)

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

  log.debug(`filtered comments.`)

  return filtered
}

require('./get')().then(comments => {
  getMembers('projectbluestreak').then(members => {
    const active = [...new Set(filter(comments, INACTIVITY).map(e => e.author.name))]

    for (let member of members) {
      log.info(`${(member.name + ':').padEnd(40)} ${active.includes(member.name) ? 'active' : 'inactive'}`)
    }
  }).catch(err => log.error(`something went wrong: ${err}`))
})
