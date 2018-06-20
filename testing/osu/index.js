const { readFileSync } = require('fs');

const beatmap = readFileSync('./map/chroma.osu', 'utf8').split('\r\n');
const hitpoints = beatmap.slice(beatmap.indexOf('[HitObjects]') + 1)
    .map((e) => {
        const a = e.split(',');

        return { x: a[0], y: a[1], time: a[2], type: a[3], sound: a[4],
            extra: (a[5] || '').split(':') };
    });