class Client {
  constructor() {
    this.data = { a: 1, b: 2 };
    this.work = require('./work').bind(this);
  }
}

// Client.prototype.work = require('./work');

const client = new Client();

client.work();