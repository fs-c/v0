const { parseRawOsu, parseRawOsr } = require('./parse');
const { onFileAdded, getColorScheme } = require('./utils');

const graph = require('./graph');
const slider = require('./slider');

const osuInput = document.getElementById('osu-input');
const osrInput = document.getElementById('osr-input');

let beatmap, replay;

const onOsuAdded = async (raw) => {
    const string = Buffer.from(raw).toString('utf8');
    beatmap = parseRawOsu(string);

    console.log(`parsed beatmap ${beatmap.metadata.artist} - ${beatmap.metadata.title}`
        + ` (${beatmap.metadata.beatmapid}) by ${beatmap.metadata.creator}`);
    console.log(beatmap);

    slider.addEvents(beatmap.events, `rgba(${getColorScheme().highlight[0]}, 0.5)`);

    graph.addActionsDensity(beatmap.events, 'Action Density in Beatmap',
        getColorScheme().highlight[0]);
};

const onOsrAdded = async (raw) => {
    replay = parseRawOsr(raw);

    console.log(`parsed replay ${replay.hash}`);
    console.log(replay);

    slider.addEvents(replay.events, `rgba(${getColorScheme().highlight[1]}, 0.5)`);

    if (beatmap) {
        graph.addEventsOffset(replay.events, beatmap.events,
            'Action Offsets of Replay', getColorScheme().highlight[1]);
    }
};

osuInput.addEventListener('input', onFileAdded(onOsuAdded));
osrInput.addEventListener('input', onFileAdded(onOsrAdded));
