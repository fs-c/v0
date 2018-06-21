const robot = require('robotjs');
const iohook = require('iohook');
const rls = require('readline-sync');
const { readFileSync } = require('fs');
const args = require('minimist')(process.argv.slice(2));

const keys = 4;
const width = 512 / keys;

const multi = args.x || 1;

const map = args.m;
const countdown = args.c || 2900;

const typeBMP = {
    circle: 1,
    combo: 4,
    hold: 128,
};

const keyMap = {
    0: 'd',
    1: 'f',
    2: 'j',
    3: 'k',
}

const tryParse = (num) => !isNaN(num) ? parseInt(num, 10) : num;

const beatmap = readFileSync(map, 'utf8').split('\r\n');
const hitpoints = beatmap.slice(beatmap.indexOf('[HitObjects]') + 1)
    .map((e) => {
        const a = e.split(',').map(tryParse);
        const extra = typeof a[5] === 'string' ?
            a[5].split(':').map(tryParse) : [];

        const type = (a[3] & typeBMP.circle) ? 
            'circle' : (a[3] & typeBMP.hold) ? 'hold' : 0;
        const column = Math.floor(a[0] / width);

        return { type, column, time: a[2], end: extra[0] };
    });

const play = (points) => {
    console.log('playing');

    for (const point of points) {
        if (!point.type)
            continue;

        const key = keyMap[point.column];
    
        setTimeout(() => {
            robot.keyToggle(key, 'down');
    
            console.log('%d/%d - down %s', Date.now(), point.time, key);
        }, point.time);

        const end = point.end || point.time + 5;

        setTimeout(() => {
            robot.keyToggle(key, 'up');
    
            console.log('%d/%d - up %s', Date.now(), end, key);
        }, end);
    }
}

// let first;
iohook.on('mousedown', (event) => {
    // console.log(event, first)

    // if (!first)
    //     first = Date.now();
    // else {
    //     console.log(Date.now() - first);
    //     first = 0;
    // }

    console.log('playing in %d', countdown);

    setTimeout(play, countdown, hitpoints);
});

iohook.start();
