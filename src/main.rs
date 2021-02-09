extern crate combine;

use rvml::parse::*;
use rvml::tokenize::*;
use rvml::eval::*;

fn main() {
    let output = eval(parse(tokenize("11 * 2 + 5 * 4")));
    println!("{:?}", output);
}
