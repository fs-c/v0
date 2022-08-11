const rb = (min, max) => (
    Math.random() * (max - min) + min
);

const rv = (val) => (rb(val * -1, val));

const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');

canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

const horizontalRoot = Math.floor(canvas.width / 2);
const verticalRoot = Math.floor(canvas.height / 2);

const segments = 5;
const radius = 100;

let points = Array(segments).fill()
    .map(() => rb(0, 2 * Math.PI)).sort()
    .map((rad) => [ Math.cos(rad) * radius, Math.sin(rad) * radius ]);

{
    ctx.beginPath();
    ctx.arc(horizontalRoot, verticalRoot, radius, 0, 2 * Math.PI);
    ctx.strokeStyle = 'gray';
    ctx.stroke();

    for (const point of points) {
        ctx.beginPath();

        ctx.arc(horizontalRoot + point[0], verticalRoot + point[1], 4, 0, 2 * Math.PI);
    
        ctx.fill();
    }
}

const horizontalDeviation = 100;
const verticalDeviation = 100;

// add dummy point to close the path
points.push(points[0]);

{
    ctx.beginPath();

    ctx.moveTo(horizontalRoot + points[0][0], verticalRoot + points[0][1]);

    for (let i = 0; i < points.length - 1; i++) {
        ctx.bezierCurveTo(
            horizontalRoot + points[i][0], verticalRoot + points[i + 1][1],
            horizontalRoot + points[i][0], verticalRoot + points[i + 1][1],
            horizontalRoot + points[i + 1][0], verticalRoot + points[i + 1][1],
        );
    }

    ctx.strokeStyle = 'red';
    ctx.stroke();
}
