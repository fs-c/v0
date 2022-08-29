const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');

canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

const segments = 40;
const segmentWidth = canvas.width / segments;

const verticalRoot = Math.floor(canvas.height / 2);

const horizontalDeviation = segmentWidth;
const verticalDeviation = 20;

const rb = (min, max) => (
    Math.floor(Math.random() * (max - min + 1) + min)
);

const rv = (val) => (rb(val * -1, val));

// {
//     ctx.moveTo(0, verticalRoot);

//     for (let i = 0; i < segments; i++) {
//         ctx.lineTo((i + 1) * segmentWidth, verticalRoot);
//     }

//     ctx.stroke();
// }

{
    ctx.moveTo(0, verticalRoot);

    let nextCp1;

    for (let i = 0; i < segments; i++) {
        const curX = i * segmentWidth;
        const nextX = curX + segmentWidth;

        const cp1 = nextCp1 || [ rb(0, horizontalDeviation) + curX, verticalRoot + rv(verticalDeviation) ];
        const cp2 = [ rb(cp1[0], nextX), verticalRoot + rv(verticalDeviation) ];

        const end = [ nextX, verticalRoot ];
        
        ctx.bezierCurveTo(...cp1, ...cp2, ...end);

        nextCp1 = [ nextX + (nextX - cp2[0]), verticalRoot + (verticalRoot - cp2[1]) ]
    }

    ctx.stroke();
}
