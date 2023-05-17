const { performance } = require('perf_hooks');

function timer() {
    // getting the command line arguments
    const regex = process.argv[2];
    const string = process.argv[3];
    // building the regex
    const re = new RegExp(regex);
    // matching the regex
    let start = performance.now();
    const result = string.match(re);
    let end = performance.now();
    return (end - start);
}

console.log(timer());
