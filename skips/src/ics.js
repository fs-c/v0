const { convertToUTC } = require('./utils');

const parseRawDate = (raw) => {
    const date = raw.split('T')[0];
    const time = raw.split('T')[1];

    if (time.length !== 7 || time[time.length - 1] !== 'Z') {
        throw new Error('unsupported time format, expected ISO 8601 UTC');
    }

    const year = date.slice(0, 4);
    const month = parseInt(date.slice(4, 6)) - 1;
    const day = date.slice(6, 8);

    const hour = parseInt(time.slice(0, 2)) + 1 % 24; // Shaky
    const minute = time.slice(2, 4);

    return { year, month, day, hour, minute,
        date: new Date(year, month, day, hour, minute) }
};

const parseIcs = async (raw) => {
    const lines = raw.split('\n').map((e) => e.trim());

    if (lines[0] !== 'BEGIN:VCALENDAR' || lines[2] !== 'VERSION:2.0') {
        throw new Error('unsupported format or version');
    }

    const segments = [];
    while (true) {
        const index = lines.indexOf('BEGIN:VEVENT');

        if (index === -1) {
            break;
        }

        segments.push([ ...lines.splice(index, 8) ].splice(1));
    }

    return segments.map((e) => e.map((e) => e.split(':')[1])).map((e) => ({
        begin: parseRawDate(e[1]),
        end: parseRawDate(e[2]),
        day: parseRawDate(e[1]).date.getDay() - 1,
        subject: e[5],
    })).reduce((acc, cur) => {
        if (!acc[cur.day])
            acc[cur.day] = [];
        acc[cur.day].push(cur);
        return acc;
    }, []);
};

exports.parseIcs = parseIcs;
