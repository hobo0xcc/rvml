extern crate combine;

use rvml::parse::*;
use rvml::tokenize::*;
use rvml::eval::*;

fn main() {
    let mut env = EvalEnv::new();
    let output = eval(parse(tokenize("let a = 20 in let b = 30 in a + b")), &mut env);
    println!("{:?}", output);
}
