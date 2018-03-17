const matrix = [];

for (let i = 0; i < 11; i++) {
  matrix.push([]);

  for (let ii = 0; ii < 15; ii++) {
    matrix[matrix.length - 1][ii] = Math.ceil(Math.random() * 9);
  }
}

let out = [];

for (let i = 0; i < matrix.length; i++) {
  for (let ii = 0; ii < matrix[i].length; ii++) {
    if (ii === 0) {
      out.push(matrix[i][ii]);
    } else {
      out.push((matrix[i][ii] === matrix[i][ii - 1]
          ? ' = '
          : matrix[i][ii] < matrix[i][ii - 1]
            ? ' > '
            : matrix[i][ii] > matrix[i][ii - 1]
              ? ' < '
              : undefined)
       + matrix[i][ii]);
    }

    if (ii === matrix[i].length - 1) {
      out.push('\n');
    }
  }

  for (let ii = 0; ii < matrix[i].length; ii++) {
    if (!matrix[i + 1]) continue;

    out.push((matrix[i][ii] === matrix[i + 1][ii]
      ? '=  '
      : '   ')
    )

    if (ii === matrix[i].length - 1) {
      out.push('\n');
    }
  }
}

console.log(out.join(''));
