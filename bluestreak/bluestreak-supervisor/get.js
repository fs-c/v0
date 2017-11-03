const Community = require('steamcommunity')
const community = new Community()

const request = require('request')

const comments = () => {
  return new Promise((resolve, reject) => {
    request.get('https://fsoc.space/api/steam/comments/103582791437945007', (err, body, data) => {
      data = JSON.parse(data)
      if (data.success) { resolve(data.comments) } else reject(data.error)
    })
  })
}

exports.comments = comments

const members = group => {
  return new Promise((resolve, reject) => {
    community.getSteamGroup(group, (err, group) => {
      if (err) return reject(err)
      console.log(`got steam group.`)

      group.getMembers((err, members) => {
        if (err) return reject(err)
        console.log(`got group members.`)

        let p = members.map(m => user(m))

        Promise.all(p)
        .then(users => {
          console.log(`got all user profiles.`)
          resolve(users)
        }).catch(err => reject(err))
      })
    })
  })
}

exports.members = members

const user = id => {
  return new Promise((resolve, reject) => {
    community.getSteamUser(id, (err, user) => {
      if (err) return reject(err)

      resolve(user)
    })
  })
}

exports.user = user

const names = () => {
  const name = id => {
    return new Promise((resolve, reject) => {
      request.get('https://steamcommunity.com/miniprofile/' + id, (err, body, data) => {

      })
    })
  }

  return new Promise((resolve, reject) => {
    request.get('https://steamcommunity.com/groups/projectbluestreak', (err, body, data) => {
      const $ = cheerio.load(data)
      let ids = [  ]

      $('.membergrid .playerAvatar').each((i, e) => ids.push(name($(e).attr('data-miniprofile'))))

      Promise.all(ids).then(resolve).catch(console.error)
    })
  })
}
