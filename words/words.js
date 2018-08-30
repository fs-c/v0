/**
 * - Common Tasks
 *      - Fetch and parse all items
 *      - Create the public/ directory, remove if it already exists
 *      - Copy static/ to public/.
 * 
 * - Build Front
 *      - Reduce items to relevant data: title, date, content snippet
 *      - Insert fetched (meta)data into Front template
 *      - Write resulting HTML to public/index.html
 * 
 * - Build Content
 *      - For every item, parse markdown to HTML
 *      - Insert HTML into Item template
 *      - Write resulting HTML to public/content/<title>.html
 */

const fs = require('fs');
const path = require('path');
const marked = require('marked');

const { parseItem } = require('./src/utils');

const cwd = require('process').cwd();

// Array of all parsed items, filter out those where parsing failed.
const items = fs.readdirSync(path.join(cwd, 'content'))
    .map((item) => path.resolve('content', item), 'utf8').map(parseItem)
    .filter((item) => item);

const publicPath = path.resolve(path.join(cwd, 'public'));
if (fs.existsSync(publicPath))
    removeFolder(publicPath);

fs.mkdirSync(publicPath);

copyFolder(path.resolve(path.join(cwd, 'static')),
    path.resolve(path.join(cwd, 'public/static')));

for (const item of items) {
    const itemPath = path.join(publicPath, path.parse(item.path).name);
    fs.mkdirSync(path.join(itemPath));
}