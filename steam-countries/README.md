# steam-countries

Steam Community scraper, gathering data on user locations. Automatically extends its ID pool and keeps track of already scraped profiles. Built to be run in parallel on multiple machines.

Expects a [RethinkDB](https://rethinkdb.com/) server to run locally on the default address and port.

```
$ rethinkdb
$ node src/index
```

The script will build the database structure if it does not already exist, but also gracefully handle and work with existing data.
