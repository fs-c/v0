const path = require('path');
const ssh = new (require('node-ssh'));

const args = (process.argv[2] || '').split('@');

if (!args[0] ||!args[1]) {
    console.error('usage: node <script> <username@host>');

    throw process.exitCode = 1;
}

const getDir = async (dir) => {
    const res = await ssh.execCommand(`ll ${dir}`);

    if (res.code) {
        throw new Error(res.stderr);
    }

    return res.stdout.split('\n').map((seg) => path.join(dir, seg));
}

(async () => {

const homedir = require('os').homedir();
const privateKey = path.join(homedir, '/.ssh/id_rsa');

await ssh.connect({
    privateKey,
    host: args[1],
    username: args[0],
});

const base = process.argv[3] || '~/public_files/';

let contents = getDir(base);

while (contents.length) {
}

console.log(contents);

})().catch(console.error);
