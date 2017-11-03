const Community = require('steamcommunity')
const community = new Community()

const get = require('./get')

const TAGS = [ 'bluestreak', 'bls' ]

const valid = user => {
	let valid = false
	for (const tag of TAGS)
		if (user.name.toLowerCase().indexOf(tag.toLowerCase()) !== -1) valid = true

	if (user.privacyState !== 'public') valid = false
	if (user.tradeBanState === 'Banned') valid = false

	return valid
}

get.members('projectbluestreak').then(members => {
  console.log(``)

  for (const member of members)
    if (!valid(member))
      console.log(`${member.name} - steamcommunity.com/${member.customURL ? `id/${member.customURL}` : `profiles/${member.steamID}`}`)
})
