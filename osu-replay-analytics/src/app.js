const { onFileAdded } = require('./utils');
const { parseRawOsu, parseRawOsr } = require('./parse');

const slider = require('./slider');

const osuInput = document.getElementById('osu-input');
const osrInput = document.getElementById('osr-input');

const onOsuAdded = async (raw) => {
    const string = Buffer.from(raw).toString('utf8');
    const beatmap = parseRawOsu(string);

    console.log(`parsed beatmap ${beatmap.metadata.artist} - ${beatmap.metadata.title}`
        + ` (${beatmap.metadata.beatmapid}) by ${beatmap.metadata.creator}`);
    console.log(beatmap);

    slider.addEvents(beatmap.events, 'rgba(255,0,0, 0.5)');
};

const onOsrAdded = async (raw) => {
    const replay = parseRawOsr(raw);

    console.log(`parsed replay ${replay.hash}`);
    console.log(replay);

    slider.addEvents(replay.events, 'rgba(0,0,255, 0.5)');
};

osuInput.addEventListener('input', onFileAdded(onOsuAdded));
osrInput.addEventListener('input', onFileAdded(onOsrAdded));
