const { Grammarly } = require('./grammarly');

const grammarly = new Grammarly({ logging: 'trace' });

grammarly.connect();

grammarly.on('ready', () => {
    const content = require('fs').readFileSync(process.argv[2], 'utf8');

    grammarly.submitText(content);
});

grammarly.on('alerts', (alerts) => {
    console.log(alerts);

    require('fs').writeFileSync('alerts.json', JSON.stringify(alerts));
});
