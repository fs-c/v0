const chalk = require('chalk');
const robot = require('robotjs');
const iohook = require('iohook');

const points = [
    { x: 200, y: 950, cur: 0, las: 0, },
    { x: 230, y: 950, cur: 0, las: 0, },
    { x: 260, y: 950, cur: 0, las: 0, },
    { x: 290, y: 950, cur: 0, las: 0, },
]

let i = 0;
setInterval(() => {
    console.time(i);
    for (let i = 0; i < points.length; i++) {
        const hx = robot.getPixelColor(points[i].x, points[i].y);

        const dif = hx - points[i].las;
        if (dif) console.log(points[i].x, hx, di);

        points[i].las = hx;
    }
    console.timeEnd(i++ - 1);    
}, 1);
