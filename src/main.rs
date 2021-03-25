extern crate clap;
extern crate combine;
extern crate inkwell;
extern crate rpds;

use clap::{App, Arg};
use rvml::alpha::*;
use rvml::closure::*;
use rvml::codegen::*;
use rvml::mono::*;
use rvml::parse::*;
use rvml::tokenize::*;
use rvml::typing::*;
use rvml::repl::*;
use std::fs;
use std::io::{self, Read};
use std::process;

fn read_source(name: &str) -> String {
    let file_opt = fs::File::open(name);
    let file = match file_opt {
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
        Ok(f) => f,
    };
    let mut f = io::BufReader::new(file);

    let mut buf = String::new();
    match f.read_to_string(&mut buf) {
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
        _ => {}
    }

    return buf;
}

fn main() -> io::Result<()> {
    let matches = App::new("rvml")
        .version("0.1.0")
        .author("hobo0xcc")
        .about("min-caml compiler with let-polymorphism")
        .arg(Arg::with_name("TARGET").long("target").help("Specify target triple").takes_value(true))
        .arg(Arg::with_name("REPL").long("repl").short("r").help("REPL"))
        .arg(
            Arg::with_name("INPUT")
                .help("Input file")
                // .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("OUTPUT")
                .help("Output file")
                .short("o")
                .long("output")
                .takes_value(true),
        )
        .get_matches();

    if matches.is_present("REPL") {
        let mut repl = Repl::new();
        repl.main_loop()?;
        return Ok(());
    }

    let input_file = matches.value_of("INPUT").unwrap();
    let source = read_source(input_file);
    if matches.is_present("AST") {
        let res = parse(tokenize(&source));
        println!("{:?}", res);
        return Ok(());
    }

    codegen(
        closure(mono(alpha(typing(parse(tokenize(&source))).0))),
        matches.value_of("OUTPUT").unwrap_or("main.o").to_string(),
        "".to_string(),
        matches.value_of("TARGET").unwrap_or("").to_string(),
    );
    Ok(())
}
