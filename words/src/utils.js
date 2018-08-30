const fs = require('fs');
const path = require('path');

const cwd = require('process').cwd();

const parseItem = exports.parseItem = (path) => {
    let file = null;
    let frontMatter = null;

    try {
        // This will throw if the file does not exist.
        file = fs.readFileSync(path, 'utf8');

        if (file[0] !== '{')
            throw new Error("Failed to find front matter");
            
        frontMatter = JSON.parse(file.slice(0, file.indexOf('}') + 1));
        
    // TODO: Maybe fall back to some defaults here?
    } catch(err) { return console.log(err); }

    // TODO: Verify that content doesn't end up to be completely bogus.
    const content = file.slice(file.indexOf('}') + 1).trim();

    return { path, content, frontMatter };
}

const copyFolder = exports.copyFolder = (from, to) => {
    if (!fs.existsSync(from))
        return console.error('copyFolder: invalid path %s', from);

    if (!fs.existsSync(to))
        fs.mkdirSync(to);

    const folder = fs.readdirSync(from)
        .map((file) => path.resolve(from, file));

    for (const file of folder) {
        if (fs.statSync(file).isDirectory()) {
            const nested = file.slice(file.lastIndexOf(path.sep));

            fs.mkdirSync(path.join(to, nested));

            copyFolder(file, nested);
        } else {
            const content = fs.readFileSync(file);
            const destination = path.join(to, path.parse(file).name);

            fs.writeFileSync(path.resolve(file, destination), content);
        }
    }
}

const removeFolder = exports.removeFolder = (path) => {
    const folder = fs.readdirSync(path);

    for (const file of folder) {
        if (fs.statSync(file).isDirectory()) {
            removeFolder(path.join(path, file));

            fs.rmdir(path);
        } else fs.unlinkSync(path.join(path, file));
    }
}