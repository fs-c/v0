const pino = require('pino');
const EventEmitter = require('events');
const SteamUser = require('steam-user');
const steamTotp = require('steam-totp');

const Client = exports.Client = class Client extends EventEmitter {
    constructor(account) {
        super();

        this.status = 'dormant';

        this.account = account;
        this.user = new SteamUser();
        this.log = pino({
            base: null,
            name: 'client',
            level: IN_DEV ? 'trace' : 'info',
        });
    }

    get state() {
        return this.status();
    }

    set state() {

    }
}
