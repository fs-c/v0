const shift = parseInt(process.argv[2], 10);
const message = process.argv[3];

let encrypted = '';
for (const char of message) {
    encrypted += String.fromCharCode(char.charCodeAt(0) + shift);
}

console.log(encrypted);
