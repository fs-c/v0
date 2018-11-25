const degToRad = exports.degToRad = (degrees) => degrees * (Math.PI / 180);

const radToDeg = exports.radToDeg = (radians) => radians * (180 / Math.PI);

const getRadius = exports.getRadius = (wheelbase, angle) =>
    wheelbase / Math.sin(degToRad(angle));

const getCirclePoint = exports.getCirclePoint = (radius, angle) => {
    const radians = degToRad(angle);

    return {
        x: radius * Math.cos(radians),
        y: radius * Math.sin(radians),
    };
};

const rotateVector = exports.rotateVector = (vectorX, vectorY, angle) => {
    const a = degToRad(angle);
    
    const acos = Math.cos(a);
    const asin = Math.sin(a);
    
    return {
        x: (acos * vectorX) + (asin * vectorY),
        y: ((asin * -1) * vectorX) + (acos * vectorY),
    };
};
