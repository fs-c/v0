const chalk = require('chalk');
const robot = require('robotjs');
const iohook = require('iohook');

const point = { x: 200, y: 950 };

let last;
setInterval(() => {
    const px = robot.getPixelColor(point.x, point.y);

    if (px !== last) {
        console.log(`new: ${chalk.hex('#' + px).bold('#######')} (${px})`);
    }

    last = px;
}, 1);
