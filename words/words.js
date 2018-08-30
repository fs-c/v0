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
const { highlight } = require('highlightjs');

const { parseItem, copyFolder, removeFolder } = require('./src/utils');

const cwd = require('process').cwd();

// Array of all parsed items, filter out those where parsing failed.
const items = fs.readdirSync(path.join(cwd, 'content'))
    .map((item) => path.resolve('content', item), 'utf8').map(parseItem)
    .filter((item) => item);

// Absolute path to the public/ folder.
const publicPath = path.resolve(path.join(cwd, 'public'));

// Remove and recreate it if it exists.
if (fs.existsSync(publicPath))
    removeFolder(publicPath);

fs.mkdirSync(publicPath);

// Copy static files from static/ into public/static/.
copyFolder(path.resolve(path.join(cwd, 'static')),
    path.resolve(path.join(cwd, 'public/static')));

const templates = {
    item: fs.readFileSync('templates/item.html', 'utf8'),
}

marked.setOptions({
    highlight: (code, lang) => highlight(lang, code).value,
});

for (const item of items) {
    // Absolute path to the folder for the item in public/.
    const itemPath = path.join(publicPath, path.parse(item.path).name);
    fs.mkdirSync(itemPath);

    const data = {
        content: marked(item.content),
        title: item.frontMatter.title,
    };

    const template = Object.keys(data).reduce((acc, cur, i) => {
        return acc.replace(`<%${cur.toUpperCase()}%>`, data[cur]);
    }, templates.item);

    fs.writeFileSync(path.join(itemPath, 'index.html'), template);
}