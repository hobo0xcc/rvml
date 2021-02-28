use crate::tokenize::*;
use crate::parse::*;
use crate::typing::*;
use crate::closure::*;
use crate::eval::*;

#[allow(dead_code)]
fn execute(input: &str) -> Object {
    return eval(closure(typing(parse(tokenize(input))).0)).unwrap();
}

#[test]
fn simple_expr1() {
    let input = "let a = 1 in let b = 2 in a + b";
    let output = execute(input);
    assert_eq!(output, Object::Int(3));
}

#[test]
fn polymorphism1() {
    let input = "let rec f x = x in if f (f (f true)) then (f (f 10)) else f 20";
    let output = execute(input);
    assert_eq!(output, Object::Int(10));
}

#[test]
fn fibonacci1() {
    let input = "let rec fib n = if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2)) in fib 15";
    let output = execute(input);
    assert_eq!(output, Object::Int(987));
}
