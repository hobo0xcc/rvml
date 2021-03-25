use std::io;
use std::io::Write;

use crate::tokenize::*;
use crate::parse::*;
use crate::typing::*;
use crate::alpha::*;
use crate::mono::*;
use crate::closure::*;
use crate::codegen::*;

enum Cmd {
    ShowType,
    ShowAST,
    ShowHelp,
    Exit,
    Execute,
}

pub struct Repl {
    
}

impl Repl {
    pub fn new() -> Repl {
        Repl {}
    }
    
    fn show_help(&self) {
        println!("REPL");
        println!("\t:t [program]\t\tShow type of program");
        println!("\t:ast [program]\t\tShow AST of program");
        println!("\t:help\t\tShow this message");
        println!("\t:exit\t\t Exit REPL");
    }

    fn command_type(&self, cmd_name: &str) -> Option<Cmd> {
        let cmd_type = match cmd_name {
            "t" => Cmd::ShowType,
            "ast" => Cmd::ShowAST,
            "help" => Cmd::ShowHelp,
            "exit" => Cmd::Exit,
            _ => return None,
        };

        Some(cmd_type)
    }

    pub fn main_loop(&mut self) -> io::Result<()> {
        self.show_help();
        'main: loop {
            print!("repl> ");
            io::stdout().flush().unwrap();
            let mut buf = String::new();
            io::stdin().read_line(&mut buf)?;
            if buf.len() == 0 {
                println!();
                return Ok(());
            }
            let input = buf.trim().to_string();
            if input.len() == 0 {
                continue;
            }

            let cmd_start = ':';
            let mut cmd_type = Cmd::Execute;
            let mut program = String::new();
            if input.chars().nth(0).unwrap() == cmd_start {
                let mut cmd_name = String::new();
                let mut program_mode = false;
                for ch in input.chars().skip(1) {
                    if program_mode {
                        program.push(ch);
                        continue;
                    }
                    if ch.is_ascii_alphanumeric() {
                        cmd_name.push(ch);
                    } else {
                        program_mode = true;
                    }
                }
                
                cmd_type = match self.command_type(&cmd_name) {
                    Some(ty) => ty,
                    None => {
                        println!("Unknown command: {}", cmd_name);
                        continue 'main;
                    }
                };
            } else {
                program = input;
            }

            match cmd_type {
                Cmd::ShowType => {
                    let ty = typing(parse(tokenize(&program))).1;
                    println!("{}", ty);
                }
                Cmd::ShowAST => {
                    let ast = parse(tokenize(&program));
                    println!("{:?}", ast);
                }
                Cmd::ShowHelp => {
                    self.show_help();
                }
                Cmd::Exit => {
                    return Ok(());
                }
                Cmd::Execute => {
                    jit_execute(closure(mono(alpha(typing(parse(tokenize(&program))).0))));
                }
            }
        }
    }
}
