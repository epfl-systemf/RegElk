use regex::Regex;
use std::env;
use std::time::{Instant};

fn main() {
    let args: Vec<String> = env::args().collect();
    let str = &args[1];
    let pat = &args[2];
    
    let reg = Regex::new(&pat).unwrap();
    let start = Instant::now();
    let firstmatch = reg.captures(&str);
    let duration = start.elapsed();
    let mut group = 0;
    for _submatch in firstmatch.iter() {
        group = group +1;
    }
    let time_nano: u128 = duration.as_nanos();
    let time_secs: f64 = (time_nano as f64) / (1000000000 as f64);
    println!("{}", time_secs);
    return;

}