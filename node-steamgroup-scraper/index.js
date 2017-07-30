const request = require('request')

const SteamCommunity = require('steamcommunity')
let community = new SteamCommunity()

require('console-stamp')(console, 'HH:MM:ss')

const fs = require('fs')

const GROUP = 'projectbluestreak'
const URL = 'https://api.myjson.com/bins/v00nr'

let x = 0

request
  .get(URL)
  .on('response', response => console.log(response.headers))

// setInterval(function() {
//   x++
//
//   if ((x % 6) === 0) {
//     fs.writeFileSync('data.json', JSON.stringify(data), 'utf-8')
//     console.log(`Saved data to disk. (${x})`)
//   }
//
//   community.getSteamGroup(GROUP, (err, group) => {
//     if (err) { console.log(err) } else console.log(`Got group.`)
//
//     let stats = {
//       members: group.members,
//       online: group.membersOnline,
//       ingame: group.membersInGame,
//       time: Date.now()
//     }
//
//     data.history.push(stats)
//   }
// }, 5 * 1000)
