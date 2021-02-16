extern crate combine;

use rvml::tokenize::*;
use rvml::parse::*;
use rvml::typing::*;
use rvml::eval::*;

fn main() {
    let output = typing(parse(tokenize("let rec f x = x in if (f true) then f 20 else f 12"))).1;
    println!("{:?}", output);
}
