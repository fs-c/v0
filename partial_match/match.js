/*

28937492174 whateverbla key something wordwhatever
keyword

*/

const string = "28937492174 whateverbla key something whatever";
const partial = "keyword";

const partialMatch = (base, partial) => {
    let concurrent = 0;
    
    for (const bc of base) {
        const pc = partial[concurrent];

        if (bc === pc)
            concurrent++;
    }

    return concurrent;
}

console.log(partialMatch(string, partial));
