extern crate combine;

use rvml::parse::*;
use rvml::tokenize::*;
use rvml::eval::*;

fn main() {
    let output = eval(parse(tokenize("if 1 then 2 * 3 else 3 * 4")));
    println!("{:?}", output);
}
