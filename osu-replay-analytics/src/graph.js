const { onColorSchemeChange } = require('./utils');

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

const actionsToChunks = (actions) => {
    const chunks = [];
    const timeFrame = 1;

    const chunksNeeded = Math.floor(actions[actions.length - 1].time / timeFrame);

    console.log('will need', chunksNeeded, 'chunks');

    for (let chunk_i = 0; chunk_i < chunksNeeded; chunk_i++) {
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

const addActionsGraph = (label, actions, color) => {
    const chunks = actionsToChunks(actions);
    graph.data.datasets.push({
        pointRadius: 0,
        pointHitRadius: 4,
        label,
        backgroundColor: `rgba(${color},0.2)`,
        borderColor: `rgba(${color},1)`,
        borderWidth: 2,
        data: chunks.map((c, i) => ({ x: i, y: c })),
    });

    graph.update();
};

exports.addActionsGraph = addActionsGraph;
