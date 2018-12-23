// Fetch the command line arguments.
const shift = 1;
const message = "Hello there!";

let encrypted = '';
// For every character in the given message...
for (const char of message) {
    // ...append a character that is shifted by the given value.
    encrypted += String.fromCharCode(char.charCodeAt(0) + shift);
}

console.log(encrypted);