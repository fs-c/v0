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
    if (!points || !points.length) {
        return;
    }

    ctx.beginPath();
    ctx.moveTo(points[0][0], points[0][1]);

    for (let i = 1; i < points.length; i++) {
        ctx.lineTo(points[i][0], points[i][1]);
    }

    ctx.stroke();
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
};

const generatePoints = (total = 10, closed = false) => {
    const points = [];

    const radianPart = (Math.PI * 2) / total; // performance stronk

    for (let i = 0; i < total; i++) {
        const radians = radianPart * i;

        points.push([
            center[0] + Math.cos(radians) * outerRadius,
            center[1] + Math.sin(radians) * outerRadius
        ]);
    }

    if (closed) {
        points.push(points[0]);
    }

    return points;
};

const points = generatePoints(5, true);

drawMarkersAt(points);
drawLineBetween(points);

drawHelpLines();
