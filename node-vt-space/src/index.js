const readline = require('readline');

const esc = '\u001B';
const int = Math.round;
const { rows, columns } = process.stdout;
// Have to wrap this to preserve context.
const write = (chunk) => process.stdout.write(chunk);

const defaults = {
    char: '#',
    player: {
        width: 5,
        height: 3,
        x: columns * 0.5,
        y: rows * 0.8,
    },
};

const cursor = {
    hide: () => write(`${esc}[?25l`),
    show: () => write(`${esc}[?25h`),
    move: (x, y) => write(`${esc}[${int(x)};${int(y)}H`),
};

const clear = {
    screen: () => write(`${esc}[2J`),
};

const draw = {
    dot: (x, y) => {
        cursor.move(Math.round(x), Math.round(y));
        write(defaults.char);
    },
    rect: (x, y, w, h, fill = false) => {
        clear.screen();

        console.log(columns, rows, x, y, w, h);

        for (let _x = 0; _x < w; _x++) for (let _y = 0; _y < h; _y++)
            if (fill || _x === 0 || _x === w - 1 || _y === 0 || _y === h - 1)
                draw.dot(x + _x, y + _y);
    },
};

const { player } = defaults;
player.draw = () => {
    const { x, y, width, height } = player;
    draw.rect(int(x - width / 2), int(y - height / 2), width, height);
};

process.stdout.on('resize', () => {
    console.log(`${process.stdout.columns}x${process.stdout.rows}`);
});

while(true) {}
