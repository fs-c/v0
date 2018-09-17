const fs = require('fs');
const { stdout } = require('process');
const stream = fs.createReadStream(null, { fd: 0 });

const cols = 8;
const colSize = 2;
const chunkSize = cols * colSize;

const replace = [ '\n', '\t', ' ' ].map((e) => e.charCodeAt(0));

stream.on('readable', () => {
    let chunk;
    let ci = 0;

    while ((chunk = stream.read(chunkSize)) !== null) {
        let string = "";

        stdout.write(
            ((ci++ * chunkSize).toString(16)).padStart(8, '0') + ": "
        );

        for (let i = 0; i < chunkSize; i++) {
            if (i < chunk.length) {
                const c = chunk[i];

                stdout.write((c.toString(16)).padStart(2, '0'));

                if (replace.includes(c)) {
                    string += '.';
                } else string += String.fromCharCode(c);
            } else stdout.write("  ");

            if (i % colSize) {
                stdout.write(" ");
            }
        }

        stdout.write(string + "\n");
    }
});