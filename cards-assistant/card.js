const log = require('winston')

module.exports = {
  parse (item) {
		if (item.type && item.type.indexOf('Trading Card') !== -1) {
			return (item.tags[1].internal_name).slice(4)
		} else return
  }
}
