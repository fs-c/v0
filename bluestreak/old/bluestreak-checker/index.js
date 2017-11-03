const SteamCommunity = require('steamcommunity')

const GROUP = 'projectbluestreak'
const TAGS = [
	'[bluestreak]',
	'[bls]'
]
const WHITELIST = [
	'76561198024447973'
]

let community = new SteamCommunity()

community.getSteamGroup(GROUP, (err, group) => {
	if (err) throw err

	group.getMembers((err, members) => {
		if (err) throw err

		console.log(`Fetched ${group.members} group members from ${GROUP}.`)
		check(members)
	})
})

function check (ids) {
	for (let SteamID of ids) {
		console.log(`Checking ${SteamID.toString()}.`)

		community.getSteamUser(SteamID, (err, user) => {
			if (err) throw err

			if (!valid(user) && !WHITELIST.includes(SteamID.toString())) {
				console.log(
					`User ${SteamID.toString()} with name ${user.name} has an invalid profile.`
				)

				// Do something with this information.
				// Contact user, remove user, whatever.
			}
		})
	}
}

function valid (user) {
	let valid = false
	for (let tag of TAGS) {
		if (user.name.toLowerCase().indexOf(tag.toLowerCase()) !== -1) valid = true
	}

	if (user.privacyState !== 'public') valid = false
	if (user.tradeBanState === 'Banned') valid = false

	return valid
}
