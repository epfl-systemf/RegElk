use regex::Regex;
use std::env;

fn main() {
   // reading args
    let args: Vec<String> = env::args().collect();
    let str = &args[1];
    let pat = &args[2];
    
    let reg = Regex::new(&pat).unwrap();
    let Some(firstmatch) = reg.captures(&str) else { 
        println!("NoMatch\n");
        return;
    };
    let mut group = 0;
    for submatch in firstmatch.iter() {
        println!("#{}:{}", group, submatch.unwrap().as_str());
        group = group +1;
    }
    println!("");
    return;

}