var port = process.env.PORT || 8080

var Gun = require('gun')

var server = require('http').createServer((req, res) => {
	if (Gun.serve(req, res)) return
})

var gun = Gun({ 
	file: 'data.json',
	web: server
})

server.listen(port)