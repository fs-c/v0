const iohook = require('iohook');

let last = 0;
iohook.on('mouseup', (event) => {
    if (last) {
        console.log(Date.now() - last);
        last = 0;
    } else last = Date.now();
});

iohook.start();
