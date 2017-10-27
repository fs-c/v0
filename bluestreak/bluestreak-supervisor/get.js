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
