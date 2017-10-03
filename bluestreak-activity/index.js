const cheerio = require('cheerio')

if (!require('fs').existsSync('body.html')) {
  const request = require('request')
  
  request.post({
    url: 'http://steamcommunity.com/comment/Clan/render/103582791437945007/-1/',
    form: {
      start: '0',
      count: '500'
    }
  }, (err, res, body) => {
    if (err) return log.error(err)
    parse(body.comments_html, handle)
  })
} else {
  parse(require('./body').comments_html, handle)
}

function parse (html, cb) {
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
      vanityURL: href.indexOf('/profiles/') === -1 ? href.slice(29) : undefined,
      id: href.indexOf('/profiles/') === -1 ? undefined : href.slice(36)
    }

    comments.push({
      id,
      author,
      text
    })
  })

  cb(comments)
}

function handle (comments) {
  console.log(`Got total of ${comments.length} comments.`)
}
