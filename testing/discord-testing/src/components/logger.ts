import * as moment from 'moment';

export function log(msg: string, ...info: any[]) {
  info = info || [];

  const date = moment().format('h:mm:ss');
  const lines = msg + ' ' + info
    .map((e = '') => JSON.stringify(e, null, 4)).join('\n');

  console.log(`[${date}] ${lines}`);
}
