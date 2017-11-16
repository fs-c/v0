const socket = io()

const app = new Vue({
  el: '#app',
  data: {
    members: [],
    online: [],
    ingame: []
  },
  mounted() {
    let ctx = $('#chart')
    let chart = new Chart(ctx, {
      type: 'line',
      data: {
        datasets: [{
          label: 'Members',
          data: this.members
        }, {
          label: 'Online',
          data: this.online,
          borderColor: '#57CBDE'
        }, {
          label: 'InGame',
          data: this.ingame,
          borderColor: '#90BA3C'
        }
      ]
      }
    })

    socket.on('update', group => {
      let d = Date.now()
      console.log(group)

      this.members.push({ y: group.members, x: d })
      this.online.push({ y: group.online, x: d })
      this.ingame.push({ y: group.ingame, x: d })

      chart.update();
    })
  }
})
