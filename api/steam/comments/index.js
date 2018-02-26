module.exports = comments

const fs = require('fs')
const data = require('./data')

const SteamID = require('steamid')

let added = {  }

function comments (req, res) {
  cleanup()

  let type
  let id = req.params.id

  try {
    let sid = new SteamID(id)

    if (sid.type === 1) type = 'Profile'
    else if (sid.type === 7) type = 'Clan'
    else throw new Error('Invalid type.')
  } catch (e) { return res.json({ success: false, error: `Failed at converting ID. (${e.message || e.msg || e})` }) }

  if (added[id] && fs.existsSync(`./steam/comments/comments_${id}`)) {
    try {
      res.json({ success: true, updated: added[id], comments: JSON.parse(fs.readFileSync(`./steam/comments/comments_${id}`)) })
    } catch(e) {
      res.json({ success: false, error: e.message || e })
      remove(id)
      return
    }
  } else {
    data.get(type, id).then(data.parse).then(comments => {
      added[id] = Date.now()

      try { // Paranoia.
        fs.writeFileSync(`./steam/comments/comments_${id}`, JSON.stringify(comments))
        res.json({ success: true, updated: added[id], comments })
      } catch (e) { throw(e) }
    }).catch(error => res.json({ success: false, error: e.message || e }))
  }
}

function cleanup (max = 24 * 60 * 60 * 1000) {
  for (let item in added) {
    if (added[item] + max < Date.now()) remove(item)
  }
}

function remove (item) {
  if (fs.existsSync(`./steam/comments/${item}_comments.json`)) {
    fs.unlinkSync(`./steam/comments/${item}_comments.json`)
  }

  delete added[item]
}
