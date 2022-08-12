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

const drawLineBetween = (points) => {
    ctx.beginPath();
    ctx.moveTo(points[0][0], points[0][1]);

    for (let i = 1; i < points.length; i++) {
        ctx.lineTo(points[i][0], points[i][1]);
    }

    ctx.stroke();
};

const getRandomUnitVector = () => {
    const radians = randomFloatBetween(0, Math.PI * 2);

    return [ Math.cos(radians), Math.sin(radians) ];
};

const getUnitVector = (vector, unit = 1) => {
    const length = Math.sqrt(Math.pow(vector[0], 2) + Math.pow(vector[1], 2));

    return [ vector[0] / length * unit, vector[1] / length * unit ];
};

const getDirectionVector = (p1, p2) => ([ p2[0] - p1[0], p2[1] - p1[1] ]);

const getPerpVector = (v, a = 0) => a ? ([ v[1], v[0] * -1 ]) : ([ v[1] * -1, v[0] ]);

const randomVectorBetween = (v1, v2) => ([
    randomFloatBetween(v1[0], v2[0]), randomFloatBetween(v1[1], v2[1]),
]);

const scaleVector = (v, x) => ([ v[0] * x, v[1] * x ]);

const drawCurveBetween = (points, oddness = 1) => {
    ctx.beginPath();
    ctx.moveTo(points[0][0], points[0][1]);

    const direction = getDirectionVector(points[0], points[1]);
    const between = randomVectorBetween(getPerpVector(direction, 0), getPerpVector(direction, 1));

    const firstControlPoint = [ points[0][0] + between[0], points[0][1] + between[1] ];

    let controlPoint = [ ...firstControlPoint ];

    for (let i = 0; i < points.length - 2; i++) {
        ctx.bezierCurveTo(...controlPoint, ...controlPoint, ...points[i + 1]);

        controlPoint = [
            (points[i + 1][0] - controlPoint[0]) + points[i + 1][0],
            (points[i + 1][1] - controlPoint[1]) + points[i + 1][1],
        ];
    }

    const lastControlPoint = [
        (points[points.length - 1][0] - firstControlPoint[0]) + points[points.length - 1][0],
        (points[points.length - 1][1] - firstControlPoint[1]) + points[points.length - 1][1],
    ];

    ctx.bezierCurveTo(
        ...controlPoint,
        ...lastControlPoint,
        ...points[points.length - 1]
    );

    ctx.fillStyle = 'red';
    ctx.fill();
};

const drawMarkersAt = (points) => {
    for (const point of points) {
        ctx.beginPath();
        ctx.arc(point[0], point[1], 3, 0, Math.PI * 2);
        ctx.fill();
    }
}

const outerRadius = 200;

const center = [ Math.floor(canvas.width / 2), Math.floor(canvas.height / 2) ];

const drawHelpLines = () => {
    ctx.beginPath();

    ctx.arc(...center, outerRadius, 0, Math.PI * 2);

    ctx.strokeStyle = 'gray';
    ctx.stroke();

    drawMarkersAt(points);
    drawLineBetween(points);
};

const generatePoints = (total = 10, closed = false) => {
    const points = [];

    const segmentRadians = (Math.PI * 2) / total;

    for (let i = 0; i < total; i++) {
        const radians = segmentRadians * i;

        points.push([
            center[0] + (Math.cos(radians) * outerRadius),
            center[1] + (Math.sin(radians) * outerRadius)
        ]);
    }

    if (closed) {
        points.push(points[0]);
    }

    return points;
};

const points = generatePoints(5, true);

drawCurveBetween(points, 1);

drawHelpLines(points);
