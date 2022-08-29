// http://www.elvenprogrammer.org/projects/bezier/reference/index.html

//
// --- colors ---
//

const colors = {
    emerald: {
        50: '#ecfdf5',
        100: '#d1fae5',
        200: '#a7f3d0',
        300: '#6ee7b7',
        400: '#34d399',
        500: '#10b981',
        600: '#059669',
        700: '#047857',
        800: '#065f46',
        900: '#064e3b',
    },
    indigo: {
        50: '#eef2ff',
        100: '#e0e7ff',
        200: '#c7d2fe',
        300: '#a5b4fc',
        400: '#818cf8',
        500: '#6366f1',
        600: '#4f46e5',
        700: '#4338ca',
        800: '#3730a3',
        900: '#312e81',
    },
};

//
// --- utils ---
//

const namespace = 'http://www.w3.org/2000/svg';

// some definitions:
// - an absolute point is relative to the top left of the drawing canvas (svg 
// element in this case)
// - a relative point is relative to some center

// convert relative to absolute point
const relToAbs = (center, point) => [ center[0] + point[0], center[1] - point[1] ];

// convert absolute to relative point
const absToRel = (center, point) => [ point[0] - center[0], center[1] - point[1] ]

// add up relative points
const add = (...points) => points.reduce((acc, cur) => {
    acc[0] += cur[0];
    acc[1] += cur[1];

    return acc;
}, [ 0, 0 ]);

// scale a relative point by some factor
const scale = (point, factor) => [
    Math.floor(point[0] * factor),
    Math.floor(point[1] * factor)
];

// length of the line between two points (magnitude of vector between them)
const length = (p1, p2) => Math.sqrt(
    Math.pow(p2[0] - p1[0], 2) + Math.pow(p2[1] - p1[1], 2)
);

// min inclusive, max exclusive
const randomFloat = (min, max) => (
    Math.random() * (max - min) + min
);

// min and max inclusive
const randomInt = (min, max) => (
    Math.floor(Math.random() * (max - min + 1) + min)  
);

// wrap an array index such that going below 0 wraps around to the maximum and 
// vice versa
const wrapIndex = (i, max) => (
    (i < 0 ? max + i : i) % max
);

// get the svg path description fragment for the given bezier curve
const getBezierDescription = (cp1, cp2, end) => (
    `C ${cp1.join(' ')}, ${cp2.join(' ')}, ${end.join(' ')} `
);

// gets the center (as an absolute point, thus relative to the elements (0,0))
const getCenter = (element) => {
    const boundingBox = element.getBoundingClientRect();
    
    return [
        Math.floor(boundingBox.width / 2),
        Math.floor(boundingBox.height / 2),
    ];
};

//
// --- visual help ---
//

const drawMarkers = (element, points, attributes = {}) => {
    const center = getCenter(element);

    for (let i = 0; i < points.length; i++) {
        const circle = document.createElementNS(namespace, 'circle');

        const absolute = relToAbs(center, points[i]);

        circle.setAttribute('cx', absolute[0]);
        circle.setAttribute('cy', absolute[1]);

        circle.setAttribute('r', 3);

        for (const attribute in attributes) {
            circle.setAttribute(attribute, attributes[attribute]);
        }

        element.appendChild(circle);

        const text = document.createElementNS(namespace, 'text');

        text.setAttribute('x', absolute[0] + 5);
        text.setAttribute('y', absolute[1] + 5);

        text.appendChild(document.createTextNode(i));

        for (const attribute in attributes) {
            text.setAttribute(attribute, attributes[attribute]);
        }

        element.appendChild(text);
    }
};

const drawPolygon = (element, points, attributes = {}) => {
    const center = getCenter(element);

    const absolutes = points.map((p) => relToAbs(center, p));

    const polygon = document.createElementNS(namespace, 'polygon');

    polygon.setAttribute('points', absolutes.map((p) => p.join(',')).join(' '));

    for (const attribute in attributes) {
        polygon.setAttribute(attribute, attributes[attribute]);
    }

    element.appendChild(polygon);
};

//
// --- main api ---
//

// generates aesthetic random points (distribute a number of points evenly along
// a circle, for each choose a random point some distance away from it)
// points are relative to some center
const generateRandomPoints = (total, { radius = 100, spread = 20 } = {}) => {
    const points = [];

    const radianSteps = Math.PI * 2 / total; // 360 degrees == 2pi radians

    for (let i = 0; i < total; i++) {
        const radians = radianSteps * i;

        points.push([
            Math.floor((Math.cos(radians) * radius)) + randomInt(-spread, spread),
            Math.floor((Math.sin(radians) * radius)) + randomInt(-spread, spread),
        ]);
    }

    return points;
};

// for every sequential pair of points (line) get the point in the middle of them
const getMiddlePoints = (points) => {
    const middles = [];

    for (let i = 0; i < points.length; i++) {
        middles.push(
            scale(add(points[i], points[wrapIndex(i + 1, points.length)]), 0.5)
        );
    }

    return middles;
};

const getAnchorPoints = (points, middlePoints) => {
    const anchors = [];

    for (let i = 0; i < points.length; i++) {
        // first line is p[i - 1] to p[i], second one is p[i] to p[i + 1]
        const l1 = length(points[wrapIndex(i - 1, points.length)], points[i]);
        const l2 = length(points[i], points[wrapIndex(i + 1, points.length)]);

        // ratio between the shorter and the longer line
        const factor = (l1 < l2 ? l1 / l2 : l2 / l1) / 2;

        const shorterMiddle = l1 < l2 ? (
            middlePoints[wrapIndex(i - 1, middlePoints.length)]
        ) : middlePoints[i];
        const longerMiddle = l1 > l2 ? (
            middlePoints[wrapIndex(i - 1, middlePoints.length)]
        ) : middlePoints[i];

        const vector = add(longerMiddle, scale(shorterMiddle, -1));

        anchors.push(add(scale(vector, factor), shorterMiddle));
    }

    return anchors;
};

const getControlPoints = (points, middlePoints, anchorPoints) => {
    const controls = [];

    for (let i = 0; i < points.length; i++) {
        const vector = add(points[i], scale(anchorPoints[i], -1));

        controls.push(add(middlePoints[wrapIndex(i - 1, middlePoints.length)], vector))
        controls.push(add(middlePoints[i], vector));
    }

    return controls;
};

const drawBlob = (element, points, {
    tension, position, ...attributes
}) => {
    const middlePoints = getMiddlePoints(points);
    const anchorPoints = getAnchorPoints(points, middlePoints);
    const controlPoints = getControlPoints(points, middlePoints, anchorPoints);
    
    let pathDescription = '';

    const firstPoint = relToAbs(position, points[0]);
    pathDescription += `M ${firstPoint[0]} ${firstPoint[1]} `;

    for (let i = 0; i < points.length; i++) {
        pathDescription += getBezierDescription(
            relToAbs(position, controlPoints[wrapIndex(i * 2 + 1, controlPoints.length)]),
            relToAbs(position, controlPoints[wrapIndex(i * 2 + 2, controlPoints.length)]),
            relToAbs(position, points[wrapIndex(i + 1, points.length)]),
        );
    }

    const path = document.createElementNS(namespace, 'path');

    path.setAttribute('d', pathDescription);

    for (const attribute in attributes) {
        path.setAttribute(attribute, attributes[attribute]);
    }

    element.appendChild(path);
};

const drawRandomBlob = (element, attributes) => {
    const points = generateRandomPoints(5, { radius: 100, spread: 50 });

    drawBlob(element, points, attributes);
};

const drawRandomTopography = (element, {
    position = getCenter(element),
    // inner radius, excluding stages and spread
    radius = 300,
    spread = radius / 3,
    tension = 1,
    stages = 5,
    // each stage should be this much larger than the preceding one (exponential growth)
    stageGrowth = 0,
    // or (!) each stage should grow by this value times the first size (constant growth)
    stageProportion = 0.1,
    colorScheme = colors.emerald,
} = {}) => {
    const points = generateRandomPoints(5, { radius, spread });

    const colorKeys = Object.keys(colorScheme);

    for (let i = stages - 1; i >= 0; i--) {
        const factor = stageGrowth ? Math.pow(stageGrowth, i) : 1 + stageProportion * i;

        drawBlob(element, points.map((p) => scale(p, factor)), {
            fill: colorScheme[colorKeys[stages - i]],
            tension, position,
        });
    }
};

//
// --- setup ---
//

const svg = document.getElementById('container').children[0];

drawRandomTopography(svg, {
    position: [ 0, 0 ],
    radius: window.innerWidth / 3,
    stageProportion: 0.2,
    colorScheme: colors.emerald,
});

drawRandomTopography(svg, {
    position: [ window.innerWidth, window.innerHeight ],
    radius: window.innerWidth / 3,
    stageProportion: 0.2,
    colorScheme: colors.emerald,
});
