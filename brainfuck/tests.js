const { bf } = require('./src');
const assert = require('assert').strict;

bf('<<< >>', (meta) => {
    assert(meta.pointer === 2, 'pointer in-/decrementation failure');
});

bf('+++ - > --- +', (meta) => {
    assert.deepEqual(meta.cells, [ 2, -2 ], 'cell in-/decrementation failure');
});

bf('+++ [ -> +< ]', (meta) => {
    assert.deepEqual(meta.cells, [ 0, 3 ], 'loop failure');
});
