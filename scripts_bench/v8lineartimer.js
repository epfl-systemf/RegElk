// getting the arguments
load('params.js')

function timer() {
    // building the regex
    const re = new RegExp(regex,"l");
    // warmup is necessary to avoid measuring compilation time
    // since the first execution calls the compiler, but not the others
    for (let i = 0; i < warmups; i++) {
	let w_match = string.match(re);
    }
    // triggering garbage collector
    gc();
    // Measuring one match
    let start = performance.now()
    let r_match = string.match(re);
    let end = performance.now();
    return ((end - start)/1000);
}

console.log(timer());
