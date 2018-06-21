const robot = require('robotjs');
const { join } = require('path');
const rls = require('readline-sync');
const { readdirSync, readFileSync } = require('fs');

const args = require('minimist')(process.argv.slice(2));

const keys = 4;
const width = 512 / keys;
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
};

const tryParse = (num) => !isNaN(num) ? parseInt(num, 10) : num;

const rawmap = readFileSync(args.m, 'utf8').split('\r\n');

const hitpoints = rawmap.slice(rawmap.indexOf('[HitObjects]') + 1)
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

        point.time = point.time - 5;

        const key = keyMap[point.column];
    
        setTimeout(() => {
            robot.keyToggle(key, 'down');
    
            console.log('%d/%d - down %s', Date.now(), point.time, key);
        }, point.time);

        const end = point.end || point.time + 3;
        setTimeout(() => {
            robot.keyToggle(key, 'up');
    
            console.log('%d/%d - up %s', Date.now(), end, key);
        }, end);
    }
};

const hook = require('iohook');

hook.on('mouseup', (event) => {
    setTimeout(play, parseInt(args.t, 10), hitpoints);
})

hook.start();
