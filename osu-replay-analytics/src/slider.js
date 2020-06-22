const { getColorScheme } = require('./utils');

const actionSlider = document.getElementById('action-slider');
const actionSlider2D = actionSlider.getContext('2d');

const { width, height } = actionSlider.getBoundingClientRect();

// Draw second marks
actionSlider2D.fillStyle = `rgba(${getColorScheme().foreground},1)`;
for (let i = 0; i < width; i += 1000) {
    actionSlider2D.fillRect(i, 0, 1, height);
    actionSlider2D.fillText(i, i + 5, height - 3);
}

// Draw 100ms marks
actionSlider2D.fillStyle = `rgba(${getColorScheme().foreground},0.1)`;
for (let i = 0; i < width; i += 100) {
    actionSlider2D.fillRect(i, 0, 1, height);
}

const addEvents = (events, color = getColorScheme().highlight[0]) => {
    actionSlider2D.fillStyle = color;

    // Draw events
    const columnHeight = Math.floor(actionSlider.getBoundingClientRect().height / 4);
    for (const event of events) {
        actionSlider2D.fillRect(event.startTime, event.column * columnHeight,
            event.endTime - event.startTime, columnHeight);
    }
};

exports.addEvents = addEvents;
