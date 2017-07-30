const request = require('request')
const cheerio = require('cheerio')

export function getNotifications (community) {
  community.getNotifications((err, notifications) => {
		if (err) {
			console.log(`Error (${err.nessage}) while getting notifications.`)
			return
		}

		console.log('Got notifications.')

	  if (notifications.comments > 0) {
	    for (let x = 0; x < notifications.comments; x++) {
	      getComment(comment => {
	        log.comments.push(comment)

	        if (log.users[comment.link]) {
	          log.users[comment.link]++
	        } else log.users[comment.link] = 1

	        console.log(`Got and logged comment by ${comment.author}`)
	      })
	    }
	  } else { console.log('No new comments found.') }
	})
}

export function getComment (callback, index = '0') {
  function trim (link) { return link.slice(link.indexOf('/id/') + 4) }

  request(URL, (err, response, html) => {
    if (err) throw err

		console.log(
			`Request to ${URL} returned response ${response.statusCode}${(html ? '.':', but no html!')}`
		)

    const $ = cheerio.load(html)

		// All of this should be reworked as it is very suboptimal.
    let comment = $('.commentthread_comments').children()[index]
    let content = $(comment).children()['1']
    let author = $(content).children().first().children()['0']
    let text = $(content).children()['1']
    let link = $(author).attr('href')

    text = $(text).text().trim()

		console.log(`New comment found, returning.`)
    callback(
      {
        author: $(author).text().trim(),
        link: trim(link),
        text
      }
    )
  })
}
