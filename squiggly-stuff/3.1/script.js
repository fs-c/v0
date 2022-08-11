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

const drawMarkersAt = (points) => {
    for (const point of points) {
        ctx.beginPath();
        ctx.arc(point[0], point[1], 3, 0, Math.PI * 2);
        ctx.fill();
    }
}

const getRandomPoint = (
    minX = 0, maxX = canvas.width, minY = 0, maxY = canvas.height
) => ([ randomIntBetween(minX, maxX), randomIntBetween(minY, maxY) ]);

const getRandomEdgePoint = () => {
    const edge = randomIntBetween(0, 3);

    switch (edge) {
        case 0: return [ randomIntBetween(0, canvas.width), 0 ];
        case 1: return [ canvas.width, randomIntBetween(0, canvas.height) ];
        case 2: return [ randomIntBetween(0, canvas.width), canvas.height ];
        case 3: return [ 0, randomIntBetween(0, canvas.height) ];
    }
};

const generatePoints = (total = 10) => {
    const points = [];

    points.push(getRandomEdgePoint());

    for (let i = 1; i < total - 1; i++) {
        points.push(getRandomPoint())
    }

    points.push(getRandomEdgePoint());

    console.log(points);

    return points;
};

const points = generatePoints(10);

drawMarkersAt(points);
drawLineBetween(points);
