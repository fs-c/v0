const sqColors = [
  'blue',
  'yellow',
  'red',
  'green',
];

const hxColors = [
  'orange',
  'red',
  'green',
  'darkviolet',
  'blue',
  'yellow',
];

let top = 0;

function simulate(left, target, top, colors) {
  let moves = 0;
  while (top !== colors.indexOf(target)) {
    moves++;
    top = (top + left ? 1 : -1) % colors.length;
  }
  return moves;
}

function tick() {
  if (gameTime === 2500) {
    top = 0;
    return console.log('skipping tick');
  }

  const colors = points <= 19 ? sqColors : hxColors;

  console.log(`top: ${colors[top]}/${top}, color: ${color}/${colors.indexOf(color)}`);
  console.log(`simulated moves: ${simulate(true, color, top, colors)}`);

  if (top !== colors.indexOf(color)) {
    turn(true);
    top = (top + 1) % colors.length;
  }
}

setInterval(tick, 600);