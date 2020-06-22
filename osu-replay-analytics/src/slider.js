const addEvents = (events, color = 'coral') => {
    const actionSlider = document.getElementById('action-slider');
    const actionSlider2D = actionSlider.getContext('2d');

    actionSlider2D.fillStyle = color;

    const columnHeight = Math.floor(actionSlider.getBoundingClientRect().height / 4);

    for (const event of events) {
        actionSlider2D.fillRect(event.startTime, event.column * columnHeight,
            event.endTime - event.startTime, columnHeight);
    }
};

exports.addEvents = addEvents;
