const robot = require('robotjs');
const cluster = require('cluster');

const points = [
    { x: 200, y: 950, },
    { x: 230, y: 950, },
    { x: 260, y: 950, },
    { x: 290, y: 950, },
];

const scan = (point) => {
    let last = 0;

    setInterval(() => {
        console.time(`${process.pid}`);

        const hx = robot.getPixelColor(point.x, point.y);

        const dif = hx - last;
        if (dif)
            console.log(point.x, hx, dif);

        last = hx;

        console.timeEnd(`${process.pid}`);
    }, 1);
};

if (cluster.isMaster) {
    for (const point of points) {
        const worker = cluster.fork({ point: JSON.stringify(point) });

        worker.send(JSON.stringify(point));
    }
} else {
    try {
        scan(JSON.parse(process.env.point));
    } catch (err) { console.error(err); }
}
