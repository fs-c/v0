const request = require('request')
const cheerio = require('cheerio')

const comments = () => {
  return new Promise((resolve, reject) => {
    request.get('https://fsoc.space/api/steam/comments/103582791437945007', (err, body, data) => {
      data = JSON.parse(data)
      if (data.success) { resolve(data.comments) } else reject(data.error)
    })
  })
}

exports.comments = comments

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

names().then(console.log)
