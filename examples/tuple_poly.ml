let rec f x =
  let rec g y =
    let rec h z =
      x, y, z in
    h in
  g in

let (ab, bb, cb) = ((f false) true) false in
let (af, bf, cf) = ((f 2.4) 3.5) 4.6 in
print_float (af +. bf +. cf);
print_newline ();
let (a, b, c) = ((f 2) 3) 4 in
print_int (a + b + c);
print_newline ();
let x = f 10 in
let y = (x 10) 3.2 in
let (a, b, c) = y in
print_int (a + b);
print_newline ();
print_float c;
print_newline ();
()
