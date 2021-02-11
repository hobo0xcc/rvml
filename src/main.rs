extern crate combine;

use rvml::parse::*;
use rvml::tokenize::*;
use rvml::eval::*;

fn main() {
    let output = eval(parse(tokenize("let rec a b c = b + c in let b = 20 in (a b) b")));
    println!("{:?}", output.unwrap());
}
