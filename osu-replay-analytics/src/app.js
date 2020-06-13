const { parseRawOsu, parseRawOsr } = require('./parse');
const { readFile, onColorSchemeChange } = require('./utils');

const Chart = require('chart.js');
const graph = new Chart('beatmap-graph', {
    type: 'line',
    data: {
        datasets: [],
    },
    options: {
        maintainAspectRatio: false,
        scales: {
            yAxes: [{
                gridLines: {
                    color: 'rgba(255, 255, 255, 0.1)',
                },
            }],
            xAxes: [{
                type: 'linear',
                position: 'bottom',
                gridLines: {
                    color: 'rgba(255, 255, 255, 0.1)',
                },
            }],
        },
    },
});

onColorSchemeChange((scheme) => {
    // TODO: This doesn't work but it's also not worth the effort to fix at the moment.

    const color = scheme === 'dark' ? 'rgba(255, 255, 255, 0.1)' : 'rgba(0, 0, 0, 0.1)';

    graph.options.scales.yAxes[0].gridLines.color = color;
    graph.options.scales.xAxes[0].gridLines.color = color;
});

const osuInput = document.getElementById('osu-input');
const osrInput = document.getElementById('osr-input');

const onFileAdded = (f) => async ({ target }) => {
    if (target.files.length > 1) {
        console.warn('multiple files selected, ignoring all but the first');
    }

    const file = target.files[0];

    if (!file) {
        throw new Error('could not get file');
    }

    const raw = await readFile(file);

    if (!raw) {
        throw new Error('could not read file');
    }

    await f(raw);
};

const actionsToChunks = (objects, timeframe) => {
    let lastTime = 0;
    let timePassed = 0;
    let cur_chunk_i = 0;
    const chunks = [0];
    for (const action of objects) {
        if (timePassed >= timeframe) {
            timePassed = 0;
            chunks[++cur_chunk_i] = 0;
        }

        timePassed += action.time - lastTime;
        chunks[cur_chunk_i]++;
        lastTime = action.time;
    }

    return chunks;
};

const timeframe = 2;

const onOsuAdded = async (raw) => {
    const string = Buffer.from(raw).toString('utf8');
    const beatmap = parseRawOsu(string);

    console.log(`parsed beatmap ${beatmap.metadata.artist} - ${beatmap.metadata.title}`
        + ` (${beatmap.metadata.beatmapid}) by ${beatmap.metadata.creator}`);

    const chunks = actionsToChunks(beatmap.hitobjects, timeframe * 1000);

    graph.data.datasets.push({
        label: 'Actions/Timeframe (.osu)',
        backgroundColor: 'rgba(255,99,132,0.2)',
        borderColor: 'rgba(255,99,132,1)',
        borderWidth: 2,
        data: chunks.map((c, i) => ({ x: (i * timeframe), y: c })),
    });

    graph.update();

    console.log(beatmap);
};

const onOsrAdded = async (raw) => {
    const replay = parseRawOsr(raw);

    console.log(`parsed replay ${replay.hash}`);

    const chunks = actionsToChunks(replay.actions, timeframe * 1000);

    graph.data.datasets.push({
        label: 'Actions/Timeframe (.osr)',
        backgroundColor: 'rgba(0, 141, 213, 0.2)',
        borderColor: 'rgba(0, 141, 213, 1)',
        borderWidth: 2,
        data: chunks.map((c, i) => ({ x: (i * timeframe), y: c })),
    });

    graph.update();

    console.log(replay);
};

osuInput.addEventListener('input', onFileAdded(onOsuAdded));
osrInput.addEventListener('input', onFileAdded(onOsrAdded));
