const DEFAULT_ID = 76561198091491690;

const Worker = exports.Worker = class {
  constructor(pool, options = {}) {
    this.pool = pool;
    this.status = 'dormant';
    this.options = Object.assign({
      interval: 10 * 1000,
    }, options);
  
    this.work();
    this.intervalID = setInterval(this.work, this.options.interval);
  }

  get state() { return this.status }
  set state(ns) {
    this.status = ns;
  }

  async work() {
    const active = await this.pool.filter({ open: true }).limit(1);

    if (!active && this.state !== 'fetching') {
      return await this.fetchIDs(DEFAULT_ID);
    }

    this.state = 'working';
  }

  async fetchIDs(base) {
    // Insert a bunch of IDs to the database.
  }
}

