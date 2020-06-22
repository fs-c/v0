class Bytes {
    constructor(buffer) {
        this.i = 0;
        this.buf = buffer;
    }

    readByte() {
        return this.buf[this.i++];
    }

    readBytes(length) {
        return this.buf.subarray(this.i, this.i += length);
    }

    readShort() {
        const res = this.buf.readInt16LE(this.i);
        this.i += 2;
        return res;
    }

    readInt() {
        const res = this.buf.readInt32LE(this.i);
        this.i += 4;
        return res;
    }

    readLong() {
        console.warn('readLong is not implemented, JS does not have native 64 bit'
            + ' int support');

        this.i += 8;

        return 0;
    }

    readULEB128() {
        let res = 0;
        let shift = 0;

        while (true) {
            const byte = this.buf[this.i++];
            res |= (byte & 0x7f) << shift;

            if ((0x80 & byte) === 0)
                break;

            shift += 7;
        }

        return res;
    }

    readUTF8String(length) {
        return this.buf.toString('utf8', this.i, this.i += length);
    }

    readOsuString() {
        if (!this.readByte()) {
            console.warn('first string byte 0x00, empty string');

            return '';
        }

        const length = this.readULEB128();
        const string = this.readUTF8String(length);

        return string;
    }
}

const decToBin = (dec) => {
    return (dec >>> 0).toString(2);
};

const parseReplayStream = (raw) => {
    const rawActions = raw.split(',');
    const events = [];
    const pressStates = new Array(10).fill(0, 0, 10);

    let lastTime = 0;
    for (let i = 0; i < rawActions.length; i++) {
        const s = rawActions[i].split('|');

        if (s[0] === '-12345') {
            // Seed is provided in s[3]

            continue;
        }

        const diff = Number(s[0]);              // long
        const x = Number(s[1]);                 // float
        const y = Number(s[2]);                 // float
        const bitwiseKeys = Number(s[3], 10);   // int

        lastTime += diff;

        if (diff < 0) {
            continue;
        }

        if (!i && !diff) {
            continue;
        }

        // TODO: Bad.
        for (let col = 0; col < 4; col++) {
            const isKeyPress = ((x | 0) & (1 << col)) > 0;

            if (pressStates[col] === 0 && isKeyPress) {
                pressStates[col] = lastTime;
            }

            if (pressStates[col] !== 0 && !isKeyPress) {
                events.push({ startTime: pressStates[col], endTime: lastTime,
                    column: col });
                pressStates[col] = 0;
            }
        }
    }

    return events.sort((a, b) => a.startTime - b.startTime);
};

const lzma = require('lzma/src/lzma_worker.js').LZMA_WORKER;

const parseRawOsr = (raw) => {
    const replay = {};

    const bytes = new Bytes(Buffer(raw));

    const mode = bytes.readByte()
    const gameVersion = bytes.readInt();
    const mapHash = bytes.readOsuString();
    const playerName = bytes.readOsuString();
    const replayHash = bytes.readOsuString();

    const threeHundreds = bytes.readShort();
    const oneHundreds = bytes.readShort();
    const fifties = bytes.readShort();

    const gekis = bytes.readShort();
    const katus = bytes.readShort();

    const misses = bytes.readShort();

    const totalScore = bytes.readInt();
    const greatestCombo = bytes.readShort();
    const perfect = bytes.readByte();

    const modsUsed = bytes.readInt();

    const lifeBar = bytes.readOsuString();

    const timeStamp = bytes.readLong();

    const lzmaLength = bytes.readInt();
    const lzmaBytes = bytes.readBytes(lzmaLength);

    const rawActions = lzma.decompress(lzmaBytes);
    const events = parseReplayStream(rawActions);

    return { mode, hash: replayHash, gameVersion, playerName, totalScore,
        perfect, events };
};

exports.parseRawOsr = parseRawOsr;
