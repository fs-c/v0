const svg = document.getElementById('svg');

svg.setAttribute('width', window.innerWidth);
svg.setAttribute('height', window.innerHeight);

// an absolute point is relative to the top left
// a relative point is relative to the center

// absolute point, relative points are relative to this
const center = [
    Math.floor(window.innerWidth / 2),
    Math.floor(window.innerHeight / 2)
];

// convert relative to absolute point
const relToAbs = (point) => [ center[0] + point[0], center[1] - point[1] ];

// convert absolute to relative point
const absToRel = (point) => [ point[0] - center[0], center[1] - point[1] ]

// add up relative points
const add = (...points) => points.reduce((acc, cur) => {
    acc[0] += cur[0];
    acc[1] += cur[1];

    return acc;
}, [ 0, 0 ]);

// scale a relative point
const scale = (point, factor) => [
    Math.floor(point[0] * factor),
    Math.floor(point[1] * factor)
];

// length of the line between p1 and p2
const length = (p1, p2) => Math.sqrt(Math.pow(p2[0] - p1[0], 2) + Math.pow(p2[1] - p1[1], 2));


const getMiddlePoints = (points) => {
    const middles = [];

    for (let i = 0; i < points.length - 1; i++) {
        middles.push(scale(add(points[i], points[i + 1]), 0.5));
    }

    middles.push(scale(add(points[points.length - 1], points[0]), 0.5));

    return middles;
};


// need to refactor this function badly
const getAnchorPoints = (points, middlePoints) => {
    const anchors = [];

    const pCopy = new Array(...points);
    const mCopy = new Array(...middlePoints);

    pCopy[-1] = points[points.length - 1];
    pCopy[points.length] = points[0];

    mCopy[-1] = middlePoints[middlePoints.length - 1];
    
    for (let i = 0; i < points.length; i++) {
        const l1 = length(pCopy[i - 1], pCopy[i]);
        const l2 = length(pCopy[i], pCopy[i + 1]);

        const ratio = (l1 > l2 ? l2 / l1 : l1 / l2) / 2;
                
        anchors.push(add(mCopy[i - 1], scale(add(mCopy[i], scale(mCopy[i - 1], -1)), ratio)));
    }

    return anchors;
}

const getControlPoints = (points, middlePoints, anchorPoints) => {
    const controls = [];

    const getIdx = (i, max) => i < 0 ? max + (i % max) : i % max;

    for (let i = 0; i < points.length; i++) {
        const vector = add(points[i], scale(anchorPoints[i], -1));

        controls.push(add(middlePoints[getIdx(i - 1, middlePoints.length)], vector))
        controls.push(add(middlePoints[i], vector));
    }

    return controls;
};

const getIdx = (i, max) => i < 0 ? max + (i % max) : i % max;

const getBezierDescription = (cp1, cp2, end) => (
    `C ${cp1.join(' ')}, ${cp2.join(' ')}, ${end.join(' ')} `
);

const getCurvePath = ({ points, ...attributes }) => {
    const middlePoints = getMiddlePoints(points);
    const anchorPoints = getAnchorPoints(points, middlePoints)
    const controlPoints = getControlPoints(points, middlePoints, anchorPoints);

    let pathDescription = '';

    const firstPoint = relToAbs(points[0]);
    pathDescription += `M ${firstPoint[0]} ${firstPoint[1]} `;

    for (let i = 0; i < points.length; i++) {
        pathDescription += getBezierDescription(
            relToAbs(controlPoints[getIdx(i * 2 + 1, controlPoints.length)]),
            relToAbs(controlPoints[getIdx(i * 2 + 2, controlPoints.length)]),
            relToAbs(points[getIdx(i + 1, points.length)]),
        );
    }

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');

    path.setAttribute('d', pathDescription);

    for (const attribute in attributes) {
        path.setAttribute(attribute, attributes[attribute]);
    }

    return path;
};

const generatePoints = (total = 3, closed = false) => {
    const outerRadius = 300;
    const innerRadius = 250;

    

    const points = [];

    const maxRadians = Math.PI * 2;
    const segmentRadians = maxRadians / total;

    for (let i = 0; i < total; i++) {
        const outerRadians = segmentRadians * i;
        const innerRadians = randomFloatBetween(0, maxRadians)

        points.push([
            Math.round(center[0] + (Math.cos(outerRadians) * outerRadius) + (Math.cos(innerRadians) * innerRadius)),
            Math.round(center[1] + (Math.sin(outerRadians) * outerRadius) + (Math.sin(innerRadians) * innerRadius)),
        ]);
    }

    if (closed) {
        points.push(points[0]);
    }

    return points.map(absToRel);
};

const curve = getCurvePath({
    points: generatePoints(),
    stroke: 'black',
    fill: 'transparent',
});

svg.appendChild(curve);
