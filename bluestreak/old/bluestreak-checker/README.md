# node-bluestreak-checker

(Very) simple script checking usernames of bluestreak group members for their validity.

[Download](https://github.com/LW2904/node-bluestreak-checker/releases/tag/1.0.0)

## Usage

You can run this script using [__NodeJS__](https://nodejs.org/en/), and download & install the prerequisites with __NPM__.

```
cd C:/your/working/directory
npm install steamcommunity
node index.js
```

## How it works

This little script is heavily dependant on the [__node-steamcommunity__](https://github.com/DoctorMcKay/node-steamcommunity) module by [DoctorMcKay](https://github.com/DoctorMcKay). It is very well documented and I recommend you check out it's wiki to learn more about the functions and properties that I'm using in my code.

I've added some comments in key parts, to explain what is happening, but I will go over the main parts here.

The first thing I do is getting the `CSteamGroup` object with `getSteamGroup(id)`. From that object I get the group members with `getMembers()` which returns an array of `SteamID` objects. Next up is the actual check, where I iterate over all of the IDs, getting their profile with `getSteamUser(id)`.
We now have a `CSteamUser` object which has a `name` property - the steam name.

On that name I then perform the valid() function which basically consists of this:
```javascript
if (name.toLowerCase().indexOf(tag.toLowerCase()) !== -1) return true
```




