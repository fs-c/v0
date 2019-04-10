const fs = require('fs');
const path = require('path');
const totp = require('steam-totp');

const home = require('os').homedir();

const dataPath = ([
    path.join(home, process.env.STEAM_DATA || '.steam.json'),
    /* Optional other paths here */
].filter((p) => fs.existsSync(p)))[0];

if (!dataPath) {
    console.error(`couldn't find data, aborting`);

    return;
}

try {
    const data = JSON.parse(fs.readFileSync(dataPath, 'utf8'));

    const name = process.argv[2] || 'default';
    const shasec = data[name].shasec;

    totp.getTimeOffset((err, off, lat) => {
        if (err)
            return console.error('err: ' + err);

        console.log(`offset of ${off}ms with ${lat}ms delay`);

        totp.getAuthCode(shasec, (err, code, off, lat) => {
            if (err)
                return console.error('err: ' + err);

            console.log(`code ${code} with ${lat}ms (off: ${off}ms) delay`);
        });
    });
} catch (err) {
    /* Either because JSON parsing failed or because data[name] doesn't exist */
    console.error(err);
}
