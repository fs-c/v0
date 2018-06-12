const find = (source, opening, closing, backwards) => {
    let igc = 0;

    source = backwards ? source.split('').reverse().join('') : source

    for (let i = 0; i < source.length; i++) {
        const c = source[i];

        if (c === opening) {
            igc++;
        } else if (c === closing) {
            if (--igc <= 0) {
                return backwards ? (source.length - 1) - i : i;
            }
        }
    }

    return -1;
}

const src = 'abcde[fgh[ijkl]mnop]qrst';
console.log(find(src, '[', ']'));
