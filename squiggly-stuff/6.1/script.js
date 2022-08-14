const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');

canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

// min inclusive, max exclusive
const randomFloatBetween = (min, max) => (
    Math.random() * (max - min) + min
);

// min and max inclusive
const randomIntBetween = (min, max) => (
    Math.floor(Math.random() * (max - min + 1) + min)  
);

const center = [ Math.floor(canvas.width / 2) - 250, Math.floor(canvas.height / 2) - 100 ];

// get a given points coordinates relative to the center
const getRelativePos = (point) => (
    [ point[0] - center[0], center[1] - point[1] ]
);

// scale a point relative to the center
const scale = (point, factor) => {
    const relative = getRelativePos(point);

    return [
        Math.round(center[0] + (relative[0] * factor)),
        Math.round(center[1] - (relative[1] * factor)),
    ];
};

// adds points together relative to the center
const add = (...points) => {
    const sum = [ 0, 0 ];

    for (const point of points) {
        const rel = getRelativePos(point);

        sum[0] += rel[0];
        sum[1] += rel[1];
    }

    return [ center[0] + sum[0], center[1] - sum[1] ];
};

// get the slope between two points relative to the center 
const slopeBetween = (p1, p2) => {
    const rp1 = getRelativePos(p1);
    const rp2 = getRelativePos(p2);

    return (rp2[1] - rp1[1]) / (rp2[0] - rp1[0])
};

const drawMarkers = (points, fillColor = 'black') => {
    ctx.save();
    ctx.strokeStyle = 'lightgray';

    for (const point of points) {
        ctx.beginPath();
        ctx.fillStyle = fillColor;
        ctx.arc(point[0], point[1], 3, 0, Math.PI * 2);
        ctx.fill();
    }

    ctx.restore();
};

const getCurveControls = (points) => {
    const controls = [ points[0] ];

    for (let i = 1; i < points.length - 1; i++) {
        controls.push(
            add(scale(points[i - 1], 1/6), scale(points[i], 2/3), scale(points[i + 1], 1/6))
        );
    }

    controls.push(points[points.length - 1]);

    return controls;
};

const drawCurve = (points) => {
    const controls = getCurveControls(points);

    ctx.beginPath();

    ctx.moveTo(...controls[0]);

    for (let i = 1; i < points.length; i++) {
        ctx.bezierCurveTo(
            ...add(scale(points[i - 1], 2/3), scale(points[i], 1/3)),
            ...add(scale(points[i - 1], 1/3), scale(points[i], 2/3)),
            ...controls[i]
        );
    }

    ctx.strokeStyle = 'red';
    ctx.stroke();
};

const points = ([ [ 0, 0 ], [ 100, 100 ], [ 220, 100 ], [ 550, 0 ], [ 600, 100 ], [ 0, 200 ] ])
    .map((p) => [ p[0] * 0.75 + center[0], -p[1] * 0.75 + center[1] ]);

drawMarkers(points, 'black');

drawMarkers(getCurveControls(points), 'red');

drawCurve(points);
