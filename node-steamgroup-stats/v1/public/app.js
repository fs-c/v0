const socket = io()

const app = new Vue({
  el: '#app',
  data: {
    group: 'projectbluestreak'
  },
  created() {
    // let ctx = $('#chart')
    // let chart = new Chart(ctx, {
    //   labels
    // })

    socket.on('update', group => {
      console.log(`Online ${group.membersOnline}`)
    })
  },
  methods: {
    newGroup: function(name) {
      socket.emit('changeGroup', name)
    }
  }
})
