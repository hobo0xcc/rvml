extern crate combine;

use rvml::parse::*;
use rvml::tokenize::*;
use rvml::eval::*;

fn main() {
    let output = eval(parse(tokenize("let rec square x = x * x in let a = 2 in square (square (square a))")));
    println!("{:?}", output.unwrap());
}
