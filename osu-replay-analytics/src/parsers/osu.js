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
    const events = [];

    for (const line of lines) {
        const fields = line.split(',');

        const x = Number(fields[0]);
        const startTime = Number(fields[2]);
        const isHold = (fields[3] & 128) ? true : false;
        const endTime = isHold ? Number(fields[5].split(':')[0]) : startTime + 25;

        const column = Math.min(Math.max(Math.floor(x * columnCount / 512), 0),
            columnCount - 1);

        if (!startTime || !endTime)
            continue;

        events.push({ startTime, endTime, column });
    }

    return events.sort((a, b) => a.startTime - b.startTime);
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

    beatmap.events = beatmap.hitobjects;

    return beatmap;
};

exports.parseRawOsu = parseRawOsu;