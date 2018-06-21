const keys = 4;
const width = 512 / keys;

const typeBMP = {
    circle: 1,
    combo: 4,
    hold: 128,
};

const tryParse = (num) => !isNaN(num) ? parseInt(num, 10) : num;

const parseMap = exports.parseMap = (raw) => {
    const lines = raw.split('\r\n');

    const objects = lines.slice(lines.indexOf('[HitObjects]') + 1).map((el) => {
        const items = el.split(',').map(tryParse);
        const extra = typeof items[5] === 'string' ? items[5].split(':').map(tryParse) : [];

        const type = (items[3] & typeBMP.circle) ? 
            'circle' : (items[3] & typeBMP.hold) ? 'hold' : 0;
        const column = Math.floor(items[0] / width);

        return { type, column, time: items[2], end: extra[0] };
    });

    return objects;
};