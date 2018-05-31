const pino = require('pino');
const EventEmitter = require('events');
const SteamUser = require('steam-user');
const steamTotp = require('steam-totp');

const STATES = {
    DORMANT: 0,
    LOGGED_ON: 1,
    ONLINE: 2,
    IN_GAME: 3,
};

const INTERVALS = {
    GAMES: 30 * 60 * 60 * 1000,
};

const Client = exports.Client = class Client extends EventEmitter {
    constructor(account) {
        super();

        this.gamesTimer = 0;

        this.status = STATES.DORMANT;

        this.account = account;
        this.user = new SteamUser();
        this.log = pino({
            base: null,
            name: 'client',
            level: IN_DEV ? 'trace' : 'info',
        });

        this.user.setOption('promptSteamGuardCode', false);

        this.user.on('error', this.log.error);
    }

    get state() {
        return this.status();
    }

    set state(ns) {
        if (this.status !== ns) {
            this.debug('state change: %s -> %s',
                humanizeState(this.state), humanizeState(ns));
        }

        this.status = ns;
    }

    humanizeState(st) {
        return Object.keys(STATES)[Object.values(STATES).indexOf(st)];
    }

    logOn() {
        this.log.debug('logging on');

        this.user.logOn(account);

        this.user.once('steamGuard', (domain, callback) => {
            this.log.debug({ domain }, 'got steamGuard event');
            
            if (domain || !this.account.shasec) {
                return;
            }

            const code = steamTotp.generateAuthCode(this.account.shasec);

            callback(code);
        });

        this.user.once('loggedOn', () => {
            this.log.info('logged on');

            this.state = STATES.LOGGED_ON;
        });

        this.user.once('webSession', (id, cookies) => {
            this.log.debug({ id, cookies }, 'got webSession event');

            this.user.setPersona(SteamUser.EPersonaState.Online);

            this.state = STATES.ONLINE;
        });
    }

    play(games) {
        this.log.debug({ games }, 'playing %d games', games.length);

        this.gamesTimer = setInterval(this.maintainGames.bind(this),
            INTERVALS.GAMES, games);
    }

    // TODO: Think of a clever name for this.
    unplay() {
        this.log.debug('clearing games interval');

        clearInterval(this.maintainGames);
    }

    maintainGames(games) {
        this.log.debug({ games }, 'maintaining games');

        this.user.gamesPlayed(games, true);

        this.state = STATES.IN_GAME;
    }
}
