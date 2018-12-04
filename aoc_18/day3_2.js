const rawInput = require('./utils').readInput(3);

const solve = (input) => {
    const ids = [];

    const rectangles = input.split('\n').map((line) => {
        const parts = line.split(' ');

        const id = parseInt(parts[0].slice(1), 10);
        const [ x, y ] = parts[2].slice(0, parts[2].length - 1).split(',')
            .map((e) => parseInt(e, 10));
        const [ width, height ] = parts[3].split('x')
            .map((e) => parseInt(e, 10));

        ids[id - 1] = true;
            
        return { x, y, width, height, id }
    });

    const points = [];    
    for (const rect of rectangles)
        for (let x = 1; x <= rect.width; x++)
            for (let y = 1; y <= rect.height; y++)
                points.push({ x: x + rect.x, y: y + rect.y, id: rect.id });

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

    for (const group of groups) {
        group.sort((a, b) => a.y - b.y);

        for (let i = 0; i < group.length - 1; i++) {
            const point = group[i];
            
            if (point.y === group[i + 1].y) {
                ids[point.id - 1] = false;
                ids[group[i + 1].id - 1] = false;
            }
        }

        if (group[group.length - 1].y === group[group.length - 2].y) {
            ids[group[group.length - 1].id - 1] = false;
            ids[group[group.length - 2].id - 1] = false;            
        }
    }

    return ids.indexOf(true) + 1;
}

console.log(solve(rawInput));

