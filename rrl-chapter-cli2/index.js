#!/usr/bin/env node

const { join } = require('path');
const { homedir } = require('os');
const process = require('process');
const marked = require('marked');
const { existsSync, writeFileSync } = require('fs');
const { RoyalRoadAPI } = require('@l1lly/royalroadl-api');

const api = new RoyalRoadAPI();

const user = existsSync(join(homedir(), '.rrl.json')) ? (
  require(join(homedir(), '.rrl.json'))
) : require(join(cwd(), '.rrl.json'));

/**
 * Reads data from stdin until 'end' is emitted.
 * 
 * @returns {Promise<string>}
 */
function readStream() { return new Promise((resolve, reject) => {
  process.stdin.setEncoding('utf8');

  let data = '';

  process.stdin.on('end', () => resolve(data));  
  process.stdin.on('readable', () => {
    const chunk = process.stdin.read();

    if (chunk !== null) {
      data += chunk;
    }
  });
})};

(async () => {

try {
  await api.user.login(user.username, user.password);
} catch(e) {
  console.error('Login failed: ' + e.message);
}

let meta = {};
const file = await readStream();

try {
  meta = JSON.parse(
    file.slice(file.indexOf('<$META') + 6, file.indexOf('$>'))
  );
} catch(e) {
  console.error('Failed parsing meta, exiting. (%o)', e.message);
  process.exit(1);
}

const content = marked(file.slice(file.indexOf('$>') + 2));

const chapter = {
  content,
  title: meta.title,
  preNote: marked(meta.preNote || '').trim(),
  postNote: marked(meta.postNote || '').trim(),
}

try {
  await api.fiction.publishChapter(meta.fictionID, chapter);
} catch(e) {
  console.error('Failed posting chapter, exiting. (%o)', e.message);
  process.exit(1);
}

console.log('Posted chapter.');

})();