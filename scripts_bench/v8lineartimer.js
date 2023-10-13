// getting the command line arguments
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
    // TODO: this should be done inside the loop but we shouldn't measure the time the GC takes
    gc();
    // Measuring the repetitions
    let start = performance.now()
    for (let i = 0; i < repetitions; i++) {
	let r_match = string.match(re);
    }
    let end = performance.now();
    return ((end - start)/1000);
}

console.log(timer());
