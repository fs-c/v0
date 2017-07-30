let index = 0

Vue.component('vocab-card', {
	props: ['vocables', 'config'],
	template: `
		<div id="vocab-card" class="container">
			<div v-for="(voc, def, index) in vocables" v-if="def == active">
				<h3 class="mb-3 sm-12">{{ def }} <small class="text-muted">{{ index+1 }}/{{ Object.keys(vocables).length }}</small></h3>
				<form class="form-inline"
							@submit.prevent="next(true)">
					<div class="form-group" :class="'has-' + status">
						<input type="text" class="form-control mb-3 sm-12"
									:placeholder="placeholder(voc.length)"
									:class="'form-control-' + status"
									v-model="answer" autofocus>
						</input>
					</div>
				</form>
			</div>
		</div>
	`,
	data: function() {
		return {
			active: Object.keys(this.vocables)[index], // Object.keys is suboptimal
			solution: this.vocables[Object.keys(this.vocables)[index]],
			answer: '',
			status: ''
		}
	},
	methods: {
		placeholder: function (x) {
			let s = ''
			for (let i = 0; i < x; i++) s += '.'
			return s
		},
		check: _.debounce(
			function () {
				let valid = true

				if (this.answer.length > this.solution.length) {
					valid = false
				}

				if (this.answer.length === 0) {
					this.status = ''
					return
				}

				for (let i in this.answer) {
					if (!(this.answer[i] === this.solution[i])) {
						valid = false
					}
				}

				// console.log('check function, valid: ' + valid)
				this.status = (valid ? 'success' : 'danger')

				if (this.answer === this.solution) {
					console.log('correct answer')
					setTimeout(this.next, this.config.delay)
					return
				}
			},
			500
		),
		next: function (skip) {
			console.log('next')
			index++

			if (skip) {
				console.log('skipped')
			}

			if (index >= Object.keys(this.vocables).length) {
				index = 0
			}

			this.active = Object.keys(this.vocables)[index]
			this.solution = this.vocables[Object.keys(this.vocables)[index]]
			this.answer = ''
			this.status = ''
		}
	},
	watch: {
		answer: function () {
			console.log('change')
			this.check()
		}
	}
})

const app = new Vue({
	el: '#app',
	data: {
		vocables: {
			// definition : vocable
			eins: 'one',
			zwei: 'two',
			drei: 'three'
		},
		config: {
			delay: 800 // Delay between correct answer and next()
		}
	},
	methods: {
		settings: function() {
			$('#settingsModal').modal('toggle')
		}
	}
})
