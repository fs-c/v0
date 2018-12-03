const rawInput = require('./utils').readInput(3);

const solve = (input) => {
    const points = [];
    const rectangles = input.split('\n').map((line) => {
        const parts = line.split(' ');
        
        const [ x, y ] = parts[2].slice(0, parts[2].length - 2).split(',')
            .map((e) => parseInt(e, 10));
        const [ width, height ] = parts[3].split('x')
            .map((e) => parseInt(e, 10));

        return { x, y, width, height }
    });

    for (const rect of rectangles) {
        for (let x = 0; x <= rect.width; x++) {
            for (let y = 0; y <= rect.height; y++) {
                points.push({ x: x + rect.x, y: y + rect.y });
            }
        }
    }

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

    let double = 0;
    for (const group of groups) {
        group.sort((a, b) => a.x - b.x);

        for (const point of group) {
            if (point.x === point.y)
                double++;
        }
    }

    return double;
}

console.log(solve(rawInput));
