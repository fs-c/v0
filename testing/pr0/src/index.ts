import * as minimist from 'minimist';
import { Pr0grammAPI, ItemFlags } from 'pr0gramm-api';

const api = Pr0grammAPI.createWithCookies();
const args = minimist(process.argv.slice(2));

(async () => {

const items = await api.items.getItems({
  promoted: true,
  flags: ItemFlags.SFW,
});

require('fs').writeFileSync('res.json', JSON.stringify(items));

})();