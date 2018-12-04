const rawInput = require('./utils').readInput(3);

const solve = (input) => {
    const rectangles = input.split('\n').map((line) => {
        const parts = line.split(' ');

        const [ x, y ] = parts[2].slice(0, parts[2].length - 1).split(',')
            .map((e) => parseInt(e, 10));
        const [ width, height ] = parts[3].split('x')
            .map((e) => parseInt(e, 10));

        return { x, y, width, height }
    });

    const points = [];    
    for (const rect of rectangles)
        for (let x = 1; x <= rect.width; x++)
            for (let y = 1; y <= rect.height; y++)
                points.push({ x: x + rect.x, y: y + rect.y });

    points.sort((a, b) => a.x - b.x);

    const groups = [];

    let groupIndex = 0;
    let last = points[0].x;
    for (const point of points) {
        if (point.x !== last) {
            groupIndex++;
        }

        if (groups[groupIndex]) {
            groups[groupIndex].push(point);
        } else groups[groupIndex] = [ point ];

        last = point.x;
    }

    let doubles = 0;
    let inDouble = false;

    for (const group of groups) {
        group.sort((a, b) => a.y - b.y);

        for (let i = 1; i < group.length; i++) {
            const point = group[i];
            const lastState = inDouble;

            inDouble = point.y === group[i - 1].y;

            if (!inDouble && lastState)
                doubles++;
        }
    }

    return doubles;
}

console.log(solve(rawInput));
