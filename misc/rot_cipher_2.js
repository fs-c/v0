// Fetch the command line arguments.
const args = process.argv.slice(2);

const shift = parseInt(args[0], 10);
const message = args[1];

let encrypted = '';
// For every character in the given message...
for (const char of message) {
    // ...append a character that is shifted by the given value.
    encrypted += String.fromCharCode(char.charCodeAt(0) + shift);
}

console.log(encrypted);