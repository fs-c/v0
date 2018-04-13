const { stdin } = require('process');

stdin.on('readable', () => {
  const chunk = stdin.read();

  console.log(chunk);
});