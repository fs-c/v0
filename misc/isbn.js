const rls = require('readline-sync');

const isValid = (isbn) => {
    let res = 0;

    for (let i = 0; i < 9; i++)
        res += parseInt(isbn[i], 10) * (i + 1);

    res %= 11;

    if (res === 10) 
        res = 'X';

    return isbn[9] == res;
}

while (true) {
    const isbn = rls.question("ISBN: ");
    const valid = isValid(isbn);

    console.log(`ISBN is ${valid ? '' : 'not '}valid`);
}
