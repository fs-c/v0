// Synchronous; should take about 2420ms.
function task() {
  for (let i = 0; i < Math.pow(2, 31); i++) {}
  return;
}

class Client {
  constructor() {}

  async login() {
    task();

    return;
  }
}

const client = new Client();

console.time();
client.login().then(console.timeEnd).catch();
