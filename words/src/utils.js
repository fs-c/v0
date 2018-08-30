const fs = require('fs');
const path = require('path');

const cwd = require('process').cwd();

const parseItem = exports.parseItem = (path) => {
    let file = null;
    let frontMatter = null;

    try {
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
        try { fs.mkdirSync(to); } catch (err) { return console.error(err); }

    const folder = fs.readdirSync(from)
        .map((file) => path.resolve(from, file));

    for (const file of folder) {
        if (fs.statSync(file).isDirectory()) {
            const segment = file.slice(file.lastIndexOf(path.sep));
            const nested = path.join(to, segment);

            if (!fs.existsSync(nested))
                try { fs.mkdirSync(nested); } catch (err) {
                    return console.error(err);
                }

            copyFolder(file, nested);
        } else {
            const content = fs.readFileSync(file);
            const destination = path.join(to, path.parse(file).name);

            try {
                fs.writeFileSync(path.resolve(file, destination), content);                
            } catch (err) { console.error(err); }
        }
    }
}

const removeFolder = exports.removeFolder = (folder) => {
    if (!fs.existsSync(folder))
        return console.error('removeFolder: invalid path %s', folder);

    const contents = fs.readdirSync(folder)
        .map((file) => path.resolve(folder, file));

    for (const file of contents) {
        if (fs.statSync(file).isDirectory()) {
            removeFolder(file);
        } else {
            fs.unlinkSync(file);
        }
    }

    try {
        fs.rmdirSync(folder);
    } catch (err) { return console.error(err); }
}