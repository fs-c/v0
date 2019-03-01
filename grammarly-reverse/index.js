const { Grammarly } = require('./grammarly');

const grammarly = new Grammarly({ logging: 'trace' });

grammarly.connect();

grammarly.on('ready', () => {
    grammarly.submitText('henlo my englisch is not god');
});

grammarly.on('alerts', (alerts) => {
    console.log(alerts);

    require('fs').writeFileSync('alerts.json', JSON.stringify(alerts));
});
