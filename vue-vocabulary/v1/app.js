function shuffle(a) {
		for (let i = a.length; i; i--) {
				let j = Math.floor(Math.random() * i)
				[a[i - 1], a[j]] = [a[j], a[i - 1]]
		}
}

Vue.component('classroom', {
	props: ['inp'],
	template: `
		<div class="container">
			<ul class="list-group">
				<li class="list-group-item" v-for="(def, voc) in vocabs">{{ def }} : {{ voc }}</li>
			</ul>
		</div>
	`,
	data: function() {
		return {
			vocabs: []
		}
	},
	created() {
		for (let [a, b] of Object.entries(inp.vocables)) {
			this.vocabs.push({ a : b })
		}

		this.vocabs = shuffle(this.vocabs)
	}
})

const app = new Vue({
	el: '#app',
	data: {
		raw: '',
		input: {
			"vocables": {
		    "eins": "one",
		    "zwei": "two",
		    "drei": "three"
		  },
		  "languages": [
		    "german",
		    "english"
		  ]
		},
		dark: false
	},
	methods: {
		set: function() {
			let temp = JSON.parse(this.raw)
			if (temp !== null && typeof temp === 'object') this.input = temp
		},
		reset: function() {
			this.raw = ''
			this.input = []
		}
	}
})
