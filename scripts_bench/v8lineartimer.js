// getting the arguments
load('scripts_bench/v8params.js')

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
    // Measuring matches, using our patched V8 for rdtsc
    let start = performance.rdtsc()
    for (let i = 0; i < repetitions; i++) {
	let r_match = string.match(re);
    }
    let end = performance.rdtsc();
    return (end-start);
}

console.log(timer());
