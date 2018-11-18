const { getRadius, getCirclePoint, radToDeg, rotateVector } = require('./common');

const inputs = [
    '1.00 3 1.00 15.00 1.00 0.00 1.00 -15.00',
];

const parseInput = (string) => {
    const numbers = string.split(' ').map((num) => parseFloat(num));

    return {
        wheelbase: numbers[0],
        segments: numbers.slice(2).reduce((acc, cur, i) => {
            if (i % 2) {
                acc[Math.floor(i / 2)].angle = cur;
            } else acc[i / 2] = { distance: cur };

            return acc;
        }, []),
    };
};

const solve = (input) => {
    for (const { angle, distance } of input.segments) {
        const radius = getRadius(input.wheelbase, angle);
        // const delta = radToDeg(distance / radius);
        // const point = getCirclePoint(radius, delta);
 
        const vector = rotateVector(radius, 0, angle);

        console.log({ angle, distance }, vector);
    }
};

for (const input of inputs) {
    const parsed = parseInput(input);

    solve(parsed);
}
