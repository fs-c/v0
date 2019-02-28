const { Grammarly } = require('./grammarly');

const grammarly = new Grammarly();

grammarly.connect({ logging: 'trace' });

grammarly.on('ready', () => {
    grammarly.submitText('henlo my englisch is not god');
});

grammarly.on('alerts', (alerts) => {
    console.log(alerts);
});
