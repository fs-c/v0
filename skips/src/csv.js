// TODO: Might as well scrap this and write a replacement
const neatCsv = require('neat-csv');

const { convertToUTC } = require('./utils');

const parseRawDate = (date, time) => {
    const sdate = date.split('.');

    if (sdate.length !== 3) {
        throw new Error(`unexpected raw date format (expected DD.MM.YYYY, got ${date})`);
    }

    const stime = time.split(':');

    if (stime.length !== 2) {
        throw new Error(`unexpected raw time format (expected HH:MM, got ${time})`);
    }

    const [ hour, minute ] = stime;
    const [ day,,year ] = sdate;

    const month = parseInt(sdate[1]) - 1

    return { year, month, day, hour, minute,
        date: new Date(year, month, day, hour, minute) };
};

const parseCsv = async (raw) => {
    const rows = await neatCsv(raw, {
        separator: '\t',
        skipLines: 1, // Since we manually specify headers
        headers: [
            'last', 'first', 'id', 'class', 'bdate', 'btime', 'edate', 'etime',
            'int', 'reason', 'textreason', 'excnr', 'status', 'exctext', 'selfreported',
        ],
    });

    if (!rows) {
        throw new Error('could not parse csv');
    }

    const student = {
        id: rows[0].id, class: rows[0].class,
        name: { first: rows[0].first, last: rows[0].last },
    };

    const absences = rows.map((e) => ({
        begin: parseRawDate(e.bdate, e.btime),
        end: parseRawDate(e.edate, e.etime),
        day: parseRawDate(e.edate, e.etime).date.getDay() - 1,
        reason: e.reason || e.textreason, // Not sure what the difference is
        excused: e.excnr !== '0', // Works so far, but rather shaky
        raw: e,
    }));

    return { student, absences };
};

exports.parseCsv = parseCsv;
