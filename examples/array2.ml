let rec f x = Array.make 1 x in
let a = f 2 in
let b = f 42.195 in
print_int (a.(0));
print_newline ();
print_float (b.(0));
print_newline ();
()
