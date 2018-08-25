const fs = require('fs');
const path = require('path');

const cwd = require('process').cwd();

const parseItem = (file) => {
    let frontMatter = null;
    
    try {
        if (file[0] !== '{')
            throw new Error("Failed to find front matter");
            
        frontMatter = JSON.parse(file.slice(0, file.indexOf('}') + 1));
    } catch(err) { console.log(err); }

    const content = file.slice(file.indexOf('}') + 1).trim();

    return { content, frontMatter };
}

// Array of absolute paths to all files in ./content.
const items = fs.readdirSync(path.join(cwd, 'content'))
    .map((item) => path.resolve('content', item));

console.log(items);

for (const item of items) {
    const { content, frontMatter } = parseItem(fs.readFileSync(item, 'utf8'));

    const publicPath = path.resolve(path.join(cwd, 'public'));
    if (fs.existsSync(publicPath))
        fs.rmdirSync(publicPath);

    fs.mkdirSync(publicPath);

    const itemPath = path.join(publicPath, path.parse(item).name);
    fs.mkdirSync(path.join(itemPath));

    
}