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

const outerRadius = 200;
const innerRadius = 50;

const center = [ Math.floor(canvas.width / 2), Math.floor(canvas.height / 2) ];

const generatePoints = (total = 10, closed = false) => {
    const points = [];

    const maxRadians = Math.PI * 2;
    const segmentRadians = maxRadians / total;

    for (let i = 0; i < total; i++) {
        const outerRadians = segmentRadians * i;
        const innerRadians = randomFloatBetween(0, maxRadians)

        points.push([
            Math.round(center[0] + (Math.cos(outerRadians) * outerRadius) + (Math.cos(innerRadians) * innerRadius)),
            Math.round(center[1] + (Math.sin(outerRadians) * outerRadius) + (Math.sin(innerRadians) * innerRadius)),
        ]);
    }

    if (closed) {
        points.push(points[0]);
    }

    return points;
};

const getRelativePos = (point) => [ point[0] - center[0], center[1] - point[1] ];

const scalePoint = (point, factor) => {
    const relative = getRelativePos(point);

    return [
        Math.round(center[0] + (relative[0] * factor)),
        Math.round(center[1] - (relative[1] * factor)),
    ];
};

const drawMarkersAt = (points) => {
    ctx.save();
    ctx.strokeStyle = 'lightgray';

    for (const point of points) {
        ctx.beginPath();
        ctx.arc(point[0], point[1], 3, 0, Math.PI * 2);
        ctx.fill();

        ctx.fillStyle = 'gray';
        ctx.fillText(`(${point[0]}, ${point[1]})`, point[0] + 5, point[1] + 4);
        ctx.fillStyle = 'black';

        const relative = getRelativePos(point);

        ctx.fillText(`(${relative[0]}, ${relative[1]})`, point[0] + 5, point[1] + 20);

        ctx.beginPath();
        ctx.moveTo(center[0], center[1]);
        ctx.lineTo(point[0], point[1]);
        ctx.stroke();
    }

    ctx.restore();
};

const drawHelpLines = () => {
    ctx.save();

    ctx.strokeStyle = 'lightgray';

    // ctx.beginPath();    
    // ctx.arc(center[0], center[1], outerRadius, 0, Math.PI * 2);
    // ctx.stroke();

    ctx.beginPath();    
    ctx.arc(center[0], center[1], outerRadius + innerRadius, 0, Math.PI * 2);
    ctx.stroke();

    ctx.beginPath();    
    ctx.arc(center[0], center[1], outerRadius - innerRadius, 0, Math.PI * 2);
    ctx.stroke();

    ctx.restore();
};

const drawLineBetween = (points) => {
    ctx.beginPath();
    ctx.moveTo(points[0][0], points[0][1]);

    for (let i = 1; i < points.length; i++) {
        ctx.lineTo(points[i][0], points[i][1]);
    }

    ctx.stroke();
};

const fillBetween = (points, color) => {
    ctx.beginPath();
    ctx.moveTo(points[0][0], points[0][1]);

    for (let i = 1; i < points.length; i++) {
        ctx.lineTo(points[i][0], points[i][1]);
    }

    ctx.fillStyle = color;

    ctx.fill();
};

const slopeBetween = (p1, p2) => {
    const rp1 = getRelativePos(p1);
    const rp2 = getRelativePos(p2);

    return (rp2[1] - rp1[1]) / (rp2[0] - rp1[0])
};

const drawCurveBetween = (points) => {
    ctx.beginPath();

    ctx.strokeStyle = 'red';



    ctx.stroke();
};

const points = generatePoints(5, true);

const colors = {
    indigo: [
        '#eef2ff',
        '#e0e7ff',
        '#c7d2fe',
        '#a5b4fc',
        '#818cf8',
        '#6366f1',
        '#4f46e5',
        '#4338ca',
        '#3730a3',
        '#312e81',
    ],
    purple: [
        '#faf5ff',
        '#f3e8ff',
        '#e9d5ff',
        '#d8b4fe',
        '#c084fc',
        '#a855f7',
        '#9333ea',
        '#7e22ce',
        '#6b21a8',
        '#581c87',
    ]
};

const iterations = 10;

for (let i = iterations - 1; i >= 0; i--) {
    const scaleFactor = 1 + (i / iterations * 2);

    fillBetween(points.map((p) => scalePoint(p, scaleFactor)), colors.purple[colors.purple.length - i - 1]);
}
