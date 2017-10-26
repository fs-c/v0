# supervisor

Some scripts to make checking for (in-)activity easier, and to generate neat little statistics and rankings.

#### How to use NodeJS

To run any JS file locally, using NodeJS, simply

1.  Install [NodeJS](https://nodejs.org/en/) (anything above 8.0 is optimal)
2.  `cd X:/your/working/directory`
3.  `node <scriptname>`

So, for example, if you were to try to run `activity.js`, you would simply `cd` into it's directory, and do `node activity`.

I plan to expand this, but for now the contents are:

### - `activity.js`

Prints a list of all members and whether or not they are considered to be active, as defined [here](https://docs.google.com/document/d/12Boaps2E0rOaAaOxz2J1FLeD77ygEB-aKElijfVKkxg/edit#heading=h.swkror9gafa4).

Note that this performs a lot of requests in a short span of time, you should not execute this more than once per minute.

### - `statistics.js`

Prints some stats to console, including a ranking of the top 10 members with the most comments, a ranking by average character count, et cetera.
