const findOpeningBrace = (source, from) => {
    const sub = source.slice(0, from);
    
    let i = sub.length - 1, ign = 0, c
    for (; i >= 0; c = sub[i--]) {
        if (c === ']')
            ign++;
        else if (c === '[')
            if (--ign < 0) return i + 1;
    }
}

const findClosingBrace = (source, from) => {
    const sub = source.slice(from);
    
    let i = 0, ign = 0, c
    for (; i <= sub.length; c = sub[i++]) {
        if (c === '[')
            ign++;
        else if (c === ']')
            if (--ign < 0) return i - 1;
    }
}

const findBrace = (source, from, closing) => {
    const sub = source.slice(...(closing ? [ from ] : [ 0, from ]));

    let i = closing ? 0 : source.length - 1, ign = 0, c
    for (; closing ? i <= sub.length : i >= 0; c = sub[closing ? i++ : i--]) {
        if (c === closing ? '[' : ']')
            ign++;
        else if (c === closing ? ']' : '[')
            if (--ign < 0) return i + closing ? -1 : 1;
    }
}