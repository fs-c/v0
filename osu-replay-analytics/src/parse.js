const parseKeyValueLines = (lines) => {
    const obj = {};

    for (const line of lines) {
        const s = line.split(':');

        if (!s[0])
            continue;

        obj[s[0].trim().toLowerCase()] = s[1].trim();
    }

    return obj;
};

const parseManiaHitObjectLines = (columnCount, lines) => {
    const actions = [];

    for (const line of lines) {
        const fields = line.split(',');

        const x = Number(fields[0]);
        const startTime = Number(fields[2]);
        const isHold = (fields[3] & 128) ? true : false;
        const endTime = isHold ? Number(fields[5].split(':')[0]) : startTime + 25;

        const column = Math.min(Math.max(Math.floor(x * columnCount / 512), 0),
            columnCount - 1);

        actions.push({ time: startTime, column });
        actions.push({ time: endTime, column });
    }

    return actions.sort((a, b) => a.time < b.time);
};

const parseRawOsu = (raw) => {
    const beatmap = {};

    const rawSections = raw.split('\r\n\r\n');

    beatmap.formatVersion = Number(rawSections.splice(0, 1)[0].slice(17));

    for (const rawSection of rawSections) {
        const lines = rawSection.split('\r\n');
        for (let i = 0; !lines[i].length; i++)
            lines.splice(i, 1);

        const name = lines.splice(0, 1)[0].slice(1, -1);

        if (!name.length)
            continue;
        
        beatmap[name.toLowerCase()] = (() => {
            const keyValueSections = [ 'General', 'Editor', 'Metadata', 'Difficulty' ];
            if (keyValueSections.includes(name))
                return parseKeyValueLines(lines);

            if (name === 'HitObjects')
                return parseManiaHitObjectLines(beatmap.difficulty.circlesize, lines);
        })();
    }

    return beatmap;
};

exports.parseRawOsu = parseRawOsu;

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
    const actions = [];

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

        if (!i && !diff) {
            continue;
        }

        if (diff < 0) {
            continue;
        }

        lastTime += diff;

        const pressed = [];
        const binaryKeys = decToBin(x).split('').reverse().join('');
        for (let i = 0; i < 10; i++) {
            pressed[i] = i < binaryKeys ? binaryKeys[i] === '1' : false;
        }

        actions.push({ time: lastTime, pressed });
    }

    return actions;
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
    const actions = parseReplayStream(rawActions).filter((act) =>
        act.pressed.reduce((acc, cur) => acc ? true : cur, false));

    return { mode, hash: replayHash, gameVersion, playerName, totalScore, perfect, actions };
};

exports.parseRawOsr = parseRawOsr;
