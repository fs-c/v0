/**
 * Required features in order of importance
 * 
 *      - Headings (# .. ######)
 *      - Bold and italic text (__, _ but also **, *)
 *      - Links ([text](url))
 *      - Code blocks with optional language (```[lang])
 *      - Inline code (`)
 *      - Quote blocks (>)
 */

 const { EOL } = require('os');

//  const parseBlock = (block) => {
//     console.log(`block: ${block}`);

//     let backtick_i = 0;
//     while ((backtick_i = block.indexOf('`')) != -1) {
        
//     }

//     if (block[0] === '#') {
//         const size = block.slice(0, 6).split('')
//             .reduce((acc, cur) => cur === '#'  ? ++acc : acc, 0);
//         const content = block.slice(size).trim();

//         console.log('   found heading (size: %d, content: "%s"', size,
//             content);

//         return `<h${size}>${content}</h${size}>`;
//     }

//     return `<p>${block}</p>`;

//     console.log();
// };

const parseBlock = (block) => {
    let html = '';

    const isHeader = block[0] === '#';
    const headerSize = block.slice(0, 6).split('')
        .reduce((acc, cur) => cur === '#'  ? ++acc : acc, 0);
    
    if (isHeader)
        block = block.slice()

    let inBlockCode = false;
    let inInlineCode = false;

    for (let i = 0; i < block.length; i++) {

    }
}

const parse = (string) => {
    let result = '';
    const blocks = string.split(EOL + EOL).map((e) => e.trim());

    for (const block of blocks) {
        result += parseBlock(block);
    }

    return result;
};

const fs = require('fs');
const html = parse(fs.readFileSync('markdown.md', 'utf8'));

fs.writeFileSync('markdown.html', html);
