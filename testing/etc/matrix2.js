const size = 10;
const matrix = [];

let out = '';

const sign = (x, y) => x > y ? '>' : x < y ? '<' : '=';

for (let i = 0; i <= size; i++) {
  matrix.push([]);

  for (let j = 0; j != size; j++) {
    matrix[i][j] = Math.ceil(Math.random() * 9);

    out += j === 0
      ? matrix[i][j]
      : ` ${sign(matrix[i][j - 1], matrix[i][j])} `
        + `${matrix[i][j]}${j === (size - 1) ? '\n' : ''}`
  }
}

console.log(matrix);
console.log();
console.log(out);
