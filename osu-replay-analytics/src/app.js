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

const actionsToChunks = (actions, timeFrame) => {
    const chunks = [];

    const chunks_needed = Math.floor(actions[actions.length - 1].time / timeFrame);

    console.log('will need', chunks_needed, 'chunks');

    for (let chunk_i = 0; chunk_i < chunks_needed; chunk_i++) {
        chunks.push(0);

        for (let action_i = 0; action_i < actions.length; action_i++) {
            const action = actions[action_i];

            if (action.time >= timeFrame * chunk_i && action.time <= timeFrame * (chunk_i + 1)) {
                chunks[chunk_i]++;
            }
        }
    }

    return chunks;
};

const timeFrame = 1;

const onOsuAdded = async (raw) => {
    const string = Buffer.from(raw).toString('utf8');
    const beatmap = parseRawOsu(string);

    console.log(`parsed beatmap ${beatmap.metadata.artist} - ${beatmap.metadata.title}`
        + ` (${beatmap.metadata.beatmapid}) by ${beatmap.metadata.creator}`);

    const chunks = actionsToChunks(beatmap.hitobjects, timeFrame * 1000);

    console.log(chunks);

    graph.data.datasets.push({
        pointRadius: 0,
        pointHitRadius: 4,
        label: 'Actions/Timeframe (.osu)',
        backgroundColor: 'rgba(255,99,132,0.2)',
        borderColor: 'rgba(255,99,132,1)',
        borderWidth: 2,
        data: chunks.map((c, i) => ({ x: i, y: c })),
    });

    graph.update();

    console.log(beatmap);
};

const onOsrAdded = async (raw) => {
    const replay = parseRawOsr(raw);

    console.log(`parsed replay ${replay.hash}`);

    const chunks = actionsToChunks(replay.actions, timeFrame * 1000);

    console.log(chunks);

    graph.data.datasets.push({
        pointRadius: 0,
        pointHitRadius: 4,
        label: 'Actions/Timeframe (.osr)',
        backgroundColor: 'rgba(0, 141, 213, 0.2)',
        borderColor: 'rgba(0, 141, 213, 1)',
        borderWidth: 2,
        data: chunks.map((c, i) => ({ x: i, y: c })),
    });

    graph.update();

    console.log(replay);
};

osuInput.addEventListener('input', onFileAdded(onOsuAdded));
osrInput.addEventListener('input', onFileAdded(onOsrAdded));
