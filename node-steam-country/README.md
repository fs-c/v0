A very naive take on scraping steam to get number of users/country.

I mostly wrote this to get some experience with [RethinkDB](https://github.com/rethinkdb/rethinkdb), which this project expects to run in the background with default settings.

- __Fire and forget.__
Assuming proper configuration, will bounce back from most errors.
- __Run once. Or a hundred times.__
Working off a "job pool" and keeping track of items that are being processed or done, across a potentially very large number of instances.
