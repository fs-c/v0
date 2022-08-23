const colors = {
    gray: {
        50: '#f9fafb',
        100: '#f3f4f6',
        200: '#e5e7eb',
        300: '#d1d5db',
        400: '#9ca3af',
        500: '#6b7280',
        600: '#4b5563',
        700: '#374151',
        800: '#1f2937',
        900: '#111827',
    },
    sky: {
        50: '#f0f9ff',
        100: '#e0f2fe',
        200: '#bae6fd',
        300: '#7dd3fc',
        400: '#38bdf8',
        500: '#0ea5e9',
        600: '#0284c7',
        700: '#0369a1',
        800: '#075985',
        900: '#0c4a6e',
    },
    blue: {
        50: '#eff6ff',
        100: '#dbeafe',
        200: '#bfdbfe',
        300: '#93c5fd',
        400: '#60a5fa',
        500: '#3b82f6',
        600: '#2563eb',
        700: '#1d4ed8',
        800: '#1e40af',
        900: '#1e3a8a',
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
    violet: {
        50: '#f5f3ff',
        100: '#ede9fe',
        200: '#ddd6fe',
        300: '#c4b5fd',
        400: '#a78bfa',
        500: '#8b5cf6',
        600: '#7c3aed',
        700: '#6d28d9',
        800: '#5b21b6',
        900: '#4c1d95',
    },
};

const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');

canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

// an absolute point is relative to the top left
// a relative point is relative to the center

// absolute point, relative points are relative to this
const center = [
    Math.floor(canvas.width / 2),
    Math.floor(canvas.height / 2)
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

// draw cartesian grid around the center
const drawGrid = ({ interval = 100 } = {}) => {
    ctx.beginPath();

    ctx.strokeStyle = colors.gray[300];

    ctx.moveTo(center[0], 0);
    ctx.lineTo(center[0], canvas.height);

    ctx.moveTo(0, center[1]);
    ctx.lineTo(canvas.width, center[1]);

    ctx.stroke();

    ctx.beginPath();

    ctx.strokeStyle = colors.gray[200];

    for (let i = 0; i < canvas.height / 2; i += interval) {
        ctx.moveTo(0, center[1] + i);
        ctx.lineTo(canvas.width, center[1] + i);

        ctx.moveTo(0, center[1] - i);
        ctx.lineTo(canvas.width, center[1] - i);
    }

    for (let i = 0; i < canvas.width / 2; i += interval) {
        ctx.moveTo(center[0] + i, 0);
        ctx.lineTo(center[0] + i, canvas.width);

        ctx.moveTo(center[0] - i, 0);
        ctx.lineTo(center[0] - i, canvas.width);
    }

    ctx.stroke();
};

const drawMarkers = ({ points, color = 'black' }) => {
    ctx.fillStyle = color;

    for (const point of points) {
        const abs = relToAbs(point);

        ctx.beginPath();
        ctx.arc(abs[0], abs[1], 3, 0, Math.PI * 2);
        ctx.fill();

        ctx.fillText(points.indexOf(point), abs[0] + 10, abs[1] + 10);
    }
};

const drawPolygon = ({ points, color }) => {
    ctx.beginPath();
    ctx.strokeStyle = color;

    const absPoints = points.map(relToAbs);

    ctx.moveTo(absPoints[0][0], absPoints[0][1]);

    absPoints.push(absPoints[0]);

    for (let i = 1; i < absPoints.length; i++) {
        ctx.lineTo(absPoints[i][0], absPoints[i][1]);
    }

    ctx.stroke();
};

const drawControlPoints = ({ controlPoints, color }) => {
    drawMarkers({ points: controlPoints, color });

    ctx.beginPath();
    ctx.strokeStyle = color;

    const points = controlPoints.map(relToAbs);
    
    for (let i = 0; i < points.length; i += 2) {
        ctx.moveTo(points[i][0], points[i][1]);
        ctx.lineTo(points[i + 1][0], points[i + 1][1]);
    }

    ctx.stroke();
};

const getMiddlePoints = (points) => {
    const middles = [];

    for (let i = 0; i < points.length - 1; i++) {
        middles.push(scale(add(points[i], points[i + 1]), 0.5));
    }

    middles.push(scale(add(points[points.length - 1], points[0]), 0.5));

    return middles;
};

const getAnchorPoints = (points, middlePoints) => {
    const anchors = [];

    // what even is this lmao

    const pCopy = new Array(...points);
    const mCopy = new Array(...middlePoints);

    pCopy[-1] = points[points.length - 1];
    pCopy[points.length] = points[0];

    mCopy[-1] = middlePoints[middlePoints.length - 1];
    
    for (let i = 0; i < points.length; i++) {
        const ratio = length(pCopy[i - 1], pCopy[i]) / length(pCopy[i], pCopy[i + 1]) / 2;
        
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

const drawCurve = ({ points, color }) => {
    const middlePoints = getMiddlePoints(points);

    //drawMarkers({ points: middlePoints, color: colors.sky[400] });
    //drawPolygon({ points: middlePoints, color: colors.sky[600] });

    const anchorPoints = getAnchorPoints(points, middlePoints)
    
    //drawMarkers({ points: anchorPoints, color: colors.sky[400] });

    const controlPoints = getControlPoints(points, middlePoints, anchorPoints);

    //drawControlPoints({ controlPoints, color: colors.indigo[400] });

    const absPoints = points.map(relToAbs);

    ctx.beginPath();

    ctx.moveTo(absPoints[0][0], absPoints[0][1]);

    for (let i = 0; i < absPoints.length; i++) {
        ctx.bezierCurveTo(
            ...relToAbs(controlPoints[getIdx(i * 2 + 1, controlPoints.length)]),
            ...relToAbs(controlPoints[getIdx(i * 2 + 2, controlPoints.length)]),
            ...absPoints[getIdx(i + 1, absPoints.length)],
        );
    }

    ctx.strokeStyle = color;
    ctx.stroke();
};

const points = [
    [ -100, -100 ],
    [ 100, 100 ],
];

const draw = ({ points }) => {
    drawGrid();
    drawMarkers({ points });
    
    drawPolygon({ points, color: colors.gray[800] });
    
    drawCurve({ points, color: colors.violet[500] });
};

draw({ points })

canvas.addEventListener('click', (e) => {
    const point = absToRel([ e.clientX, e.clientY ]);

    const closestIndex = points.reduce((acc, cur, i) => {
        const dist = length(cur, point);

        if (dist < acc[0]) {
            acc[0] = dist;
            acc[1] = i;
        }

        return acc;
    }, [ Infinity, 0 ])[1];

    points.splice(closestIndex + 1, 0, point);

    canvas.width = canvas.width;

    draw({ points });
});
