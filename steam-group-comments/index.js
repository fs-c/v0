// @Jarzon's fork of the original version, found here:
// https://github.com/Jarzon/node-steamcommunity
const SteamCommunity = require('../resources/node-steamcommunity-master/index.js')

// scraper.js not needed anymore.
// import { getNotifications, getComment } from 'src/scraper.js'

const GROUPID = '103582791437945007'
const GROUP = 'projectbluestreak'
const URL = 'http://steamcommunity.com/groups/' + GROUP
const ACCOUNT = {
	accountName: 'fsoc10',
	password: '2GFiWQDF4V'
}

let community = new SteamCommunity()

let log = {
  comments: [],
  users: {}
}

community.login(ACCOUNT, err => {
  if (err) throw err
  console.log(`Logged in with ${ACCOUNT.accountName}.`)
})
