const gun = Gun(location.origin + '/gun');
const messages = gun.get('messages');

const app = new Vue({
  el: '#app',
  data: {
    message: '',
    messages: {},
  },
  methods: {
    submit(event) {
      console.log(`submitting message ${this.message}`);
      messages.set({ text: this.message });
    },
    remove(key) {
      const message = this.messages[key];

      console.log(`removing message ${key} / ${message.text}`)
      messages.unset(message);
    },
  },
});

messages.map().on((data, key) => {
  Vue.set(app.messages, key, data);
});
