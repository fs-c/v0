const payloads = function() { return {
  resume: {
    op: 1,
    d: this.token,
  },
}};

class Client {
  constructor() {
    this.token = 'token';
  }

  get payloads() { return payloads.bind(this)() }
}

const client = new Client();

console.log(client.payloads.resume);
