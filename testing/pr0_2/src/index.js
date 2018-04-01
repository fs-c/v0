const { api } = require('./pr0');
const { log } = require('@sturmwalzer/logger');

(async () => {

log(await api.items.newest());

})();