extern crate combine;
extern crate inkwell;

use rvml::tokenize::*;
use rvml::parse::*;
use rvml::typing::*;
use rvml::eval::*;
use rvml::codegen::*;
use io::Write;
use std::io;

#[allow(dead_code)]
fn repl() -> io::Result<()> {
    loop {
        let mut source = String::new();
        print!(">> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut source)?;
        let output = eval(typing(parse(tokenize(source.trim()))).0);
        println!("{:?}", output.unwrap());
    }
}

fn main() -> io::Result<()> {
    // repl()?;
    let mut source = String::new();
    print!("input>> ");
    io::stdout().flush()?;
    io::stdin().read_line(&mut source)?;
    codegen(
        typing(
            parse(
                tokenize(
                    source.trim()
                )
            )
        ).0,
        "main.o".to_string(),
        "".to_string(),
        "".to_string(),
    );
    Ok(())
}
