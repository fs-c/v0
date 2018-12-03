const fs = require('fs');

const readInput = exports.readInput = (day) => {
    return fs.readFileSync(`day${day}_input.txt`, 'utf8');
};

const readExample = exports.readExample = (day) => {
    return fs.readFileSync(`day${day}_example.txt`, 'utf8');
};

exports.sortedArray = {};

exports.sortedArray.insert = (arr, val) => {
    let mid, lo = 0, hi = arr.length;

    while (lo < hi) {
        mid = (lo + hi) >>> 1;

        if (arr[mid] < val)
            lo = mid + 1;
        else hi = mid;
    }

    arr.splice(lo, 0, val);
};

exports.sortedArray.find = (arr, val) => {
    let mid, lo = 0, hi = arr.length;

    while (lo < hi) {
        mid = (lo + hi) >>> 1;

        if (arr[mid] === val)
            return mid;

        if (arr[mid] < val)
            lo = mid + 1;
        else hi = mid;
    }

    return -1;
}
