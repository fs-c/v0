# `grammarly-reverse`

Playing around with the free Grammarly WebSocket.

```js
// Optionally, you could provide `cookies` and [WebSocket-]`client` as well
const grammarly = new Grammarly({ logging: 'trace' });

grammarly.connect();

grammarly.on('connectionError', (err) => {
    // Error from the WebSocket connection
});

grammarly.on('connectError', (err) => {
    // Error connecting to grammarly.com or the grammarly WebSocket
});

grammarly.on('ready', () => {
    const content = require('fs').readFileSync('input.txt', 'utf8');

    grammarly.submitText(content);
});

grammarly.on('alerts', (alerts) => {
    require('fs').writeFileSync('alerts.json', JSON.stringify(alerts));
});
```