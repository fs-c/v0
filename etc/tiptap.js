const colors = [
  'blue',
  'yellow',
  'red',
  'green',
];

let top = 0;

function tick() {
  if (gameTime === 2500) {
    top = 0;
    return console.log('skipping tick');
  }

  console.log(`top: ${colors[top]}/${top}, color: ${color}/${colors.indexOf(color)}`);

  if (top !== colors.indexOf(color)) {
    turn(true);
    top = (top + 1) % 4;
  }
}

setInterval(tick, 750);