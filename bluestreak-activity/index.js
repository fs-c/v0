const request = require('request')
const cheerio = require('cheerio')

const log = require('../logger')

// request.post({
//   url: 'http://steamcommunity.com/comment/Clan/render/103582791437945007/-1/',
//   form: {
//     start: '0',
//     count: '500'
//   }
// }, (err, res, body) => {
//   if (err) return log.error(err)
//   parse(body)
// })

let file = require('fs').readFileSync('comments_html.html', { encoding: 'utf-8' })

// function parse (body) {
//   try { body = JSON.parse(body) } catch (e) { return log.error(e) }
//   const $ = cheerio.load(body.comments_html)
//
//   $('.commentthread_comment_content').each(i => {
//     console.log($(this).children('.commentthread_comment_text').text())
//   })
// }
