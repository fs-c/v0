const cheerio = require('cheerio')
const request = require('request')

// BLS: 103582791437945007

get('103582791437945007', '0', '200')
.catch(err => console.error(`ERROR GETTING COMMENTS: ${err}`))
.then(parse).then(comments => {
  console.log(comments)
})

function get (id, start, count, cb) {
  return new Promise((resolve, reject) => {
    request.post({
      url: `http://steamcommunity.com/comment/Clan/render/${id}/-1/`,
      form: { start, count }
    }, (err, res, body) => {
      if (err) reject(err)
      // Write res body to disk and get it again to take advantage of require()s parsing,
      // remove file after callback.
      require('fs').writeFileSync('body.json', body)
      resolve(require('./body.json').comments_html)
      require('fs').unlinkSync('body.json')
    })
  })
}

function parse (html, cb) {
  return new Promise((resolve, reject) => {
    let comments = []

    const $ = cheerio.load(html)

    $('.commentthread_comment').each((i, e) => {
      const $content = $(e).children('.commentthread_comment_content')
      const $author = $content.children('.commentthread_comment_author')

      let id = $content.children('.commentthread_comment_text').attr('id').slice(16).trim()

      let text = $content.children('.commentthread_comment_text').text().trim()
      let href = $author.children('.commentthread_author_link').attr('href')

      let author = {
        name: $author.children('.commentthread_author_link').text().trim(),
        vanityURL: href.indexOf('/profiles/') === -1 ? href.slice(29) : false,
        id: href.indexOf('/profiles/') === -1 ? undefined : href.slice(36)
      }

      comments.push({
        id,
        author,
        text
      })
    })

    resolve(comments)
  })
}
