extern crate combine;

use rvml::parse::*;
use rvml::tokenize::*;
use rvml::eval::*;

fn main() {
    let mut env = EvalEnv::new(None);
    let output = eval(parse(tokenize("let rec a b c = b + c in (a 2) (a 3 3)")), &mut env);
    println!("{:?}", output);
}
