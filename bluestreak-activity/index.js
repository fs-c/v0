const cheerio = require('cheerio')
const request = require('request')

const fs = require('fs')

// BLS: 103582791437945007

get('103582791437945007', '0', '100')
.then(body => parse(body.comments_html))
.then(comments => fs.writeFileSync('comments.json', JSON.stringify(comments)))
.catch(err => console.error(`something went wrong: ${err}`)

// Gets `count` comments starting from `start` of the group `id`.
function get (id, start, count) {
  return new Promise((resolve, reject) => {
    request.post({
      url: `http://steamcommunity.com/comment/Clan/render/${id}/-1/`,
      form: { start, count }
    }, (err, res, body) => {
      if (err) reject(err)
      // Write res body to disk and get it again to take advantage of require()s parsing,
      // remove file after callback.
      console.log(`got response from steamcommunity.com (${res.statusCode})`)

      fs.writeFileSync('body.json', body)
      resolve(require('./body.json'))
      fs.unlinkSync('body.json') // Delete file after we read it.
    })
  })
}

// Parses html and returns an array of comments.
function parse (html) {
  return new Promise((resolve, reject) => {
    let comments = []

    const $ = cheerio.load(html)

    $('.commentthread_comment').each((i, e) => {
      const $content = $(e).children('.commentthread_comment_content')
      const $avatar = $(e).children('.commentthread_comment_avatar')
      const $author = $content.children('.commentthread_comment_author')

      let id = $content.children('.commentthread_comment_text').attr('id').slice(16).trim()

      let text = $content.children('.commentthread_comment_text').text().trim()
      let href = $author.children('.commentthread_author_link').attr('href')

      let date = new Date($author.children('.commentthread_comment_timestamp').attr('title'))

      let author = {
        name: $author.children('.commentthread_author_link').text().trim(),
        customURL: href.indexOf('/id/') !== -1 ? href.slice(29) : false, // It's ok if this isn't here.
        id: href.indexOf('/profiles/') !== -1 ? href.slice(36) : undefined, // Undefined because we should get this later.
        avatar: $avatar.children('a').children('img').attr('src'),
        rank: $author.children('img').attr('title') || 'Member'
      }

      comments.push({
        id,
        author,
        text,
        date
      })
    })

    resolve(comments)
  })
}
