const args = process.argv.slice(2);

const convertArgs = (args) => {
  return args.reduce((acc, cur, i, arr) => {
    console.log(acc, cur, arr[i - 1]);

    if (cur[0] === '-' && cur[1] !== '-') {
      acc[cur.slice(1)] = true;

      return acc;
    }

    if (cur.slice(0, 2) === '--') {
      acc[cur.slice(2)] = undefined;

      return acc;
    }

    acc[arr[i - 1].slice(2)] = cur;

    return acc;
  }, {})
}

console.log(convertArgs(args))
