# supervisor

Some scripts to make checking for (in-)activity easier, and to generate neat little statistics and rankings.

#### How to use NodeJS

To run any JS file locally using NodeJS, simply

1.  Install [NodeJS](https://nodejs.org/en/) (anything above 8.0 is optimal)
2.  `cd X:/your/working/directory/`
3.  If this is the first time starting this script, `npm i`, to download dependencies. This will only work if you also have the `package.json` file in your project directory.
4.  `node <scriptname>`

So, for example, if you were to want to run `activity.js`, you would simply `cd` into it's directory, and do `node activity` (obviously after having already `npm i`d once).

## Contents

I plan to expand this, but for now the scripts available are:

### - `activity.js`

Prints all members that are considered inactive, as defined [here](https://docs.google.com/document/d/12Boaps2E0rOaAaOxz2J1FLeD77ygEB-aKElijfVKkxg/edit#heading=h.swkror9gafa4).

Note that this performs a lot of requests in a short span of time, take care to not get [429](https://tools.ietf.org/html/rfc6585#page-3)d. _I'm working on that._

### - `statistics.js`

Prints some stats to console, including a ranking of the top 10 members with the most comments, a ranking by average character count, et cetera.

### - `valid.js`

Prints all members with an invalid tag, as defined in the rules.
