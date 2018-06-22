const { join } = require('path');
const { homedir } = require('os');
const { parseMap } = require('./parseMap');
const { readdirSync, readFileSync } = require('fs');

const args = require('minimist')(process.argv.splice(2));

const searchTags = (args.m || '').split(',');
const songsPath = (args.d || '~/osufolder/Songs/').replace('~', homedir());
const beatmaps = readdirSync(songsPath)
    .map((song) => readdirSync(join(songsPath, song))
        .map((el) => join(songsPath, song, el)))
    .reduce((ac, cu) => { ac.push(...cu); return ac; }, [])
    .filter((el) => searchTags.map((tg) => el.includes(tg))
        .reduce((ac, cu) => !cu ? ac = cu : ac, true));

if (!beatmaps.length) {
    console.error('no beatmaps matching %s found', searchTags.join());
    return 0;
}

const hitpoints = parseMap(readFileSync(beatmaps[0], 'utf8'));
