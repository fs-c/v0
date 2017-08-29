const log = require('winston')

const SLIM_ONLY = process.env.SLIM_ONLY === 'true'
log.warn(`process env SLIM_ONLY set to ${SLIM_ONLY}`)

const fs = require('fs')
const PATH = './comments/'

const KEY = require('../../steamdata.json').main.apikey
const steamid = require('steamidconvert')(KEY)

let comments = require('../comments/comments.json')
let slim = require('../comments/slim.json')

let ids = []
for (let c of comments) { ids.push(c.id) }

slim.start = Date.now()
fs.writeFileSync(PATH + 'slim.json', JSON.stringify(slim))
log.debug(`wrote to slim.json`)

module.exports = {
  add (cmts) {
    for (let comment of cmts) {
      if (ids.includes(comment.id)) {
        slim['updated'] = Date.now()
        log.debug(`logged last updated time (${Date.now()}) to slim.json.`)
        fs.writeFileSync(PATH + 'slim.json', JSON.stringify(slim))
        log.debug(`wrote to slim.json`)
        return log.info(`found already logged comment - stopping loop.`)
      }

      log.silly(`found new comment.`)
      ids.push(comment.id)

      // YES, this is ugly, but convertVanity() is async, so what
      // are you going to do about it.
      if (comment.author.vanityURL) {
        steamid.convertVanity(comment.author.vanityURL, (err, res) => {
          if (err) return log.error(`SteamIDConverter`, err)

          log.debug(`converted ${comment.author.vanityURL} to ${res}.`)

          comment.author.id = res.toString(10)

          if (!SLIM_ONLY) {
            comments.push(comment)

            fs.writeFileSync(PATH + 'comments.json', JSON.stringify(comments))
            log.debug(`wrote to comments.json`)
          }

          if (slim[comment.author.id]) {
            slim[comment.author.id]++
          } else slim[comment.author.id] = 1

          fs.writeFileSync(PATH + 'slim.json', JSON.stringify(slim))
          log.debug(`wrote to slim.json`)
        })
      } else {
        comments.push(comment)
        if (!SLIM_ONLY) {
          fs.writeFileSync(PATH + 'comments.json', JSON.stringify(comments))
          log.debug(`wrote to comments.json`)
        }

        fs.writeFileSync(PATH + 'slim.json', JSON.stringify(slim))
        log.debug(`wrote to slim.json`)
      }
    }

    slim['updated'] = Date.now()
    log.debug(`logged last updated time (${Date.now()}) to slim.json.`)
    fs.writeFileSync(PATH + 'slim.json', JSON.stringify(slim))
    log.debug(`wrote to slim.json`)
  }
}
