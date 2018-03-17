const log = require('winston')

module.exports = {
  // TODO: A card class with some getters would be useful.
  parse (item) {
		if (item.type && item.type.indexOf('Trading Card') !== -1) {
			return (item.tags[1].internal_name).slice(4)
		} else return
  }
}
