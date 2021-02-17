extern crate combine;

use rvml::tokenize::*;
use rvml::parse::*;
use rvml::typing::*;
use rvml::eval::*;
use io::Write;
use std::io;

fn repl() -> io::Result<()> {
    loop {
        let mut source = String::new();
        print!(">> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut source)?;
        let output = eval(typing(parse(tokenize(source.trim()))).0);
        println!("{:?}", output.unwrap());
    }

    Ok(())
}

fn main() -> io::Result<()> {
    repl()?;
    Ok(())
    // let output = typing(parse(tokenize("let rec f x = x in if (f true) then f 20 else f 12"))).1;
    // println!("{:?}", output);
}
