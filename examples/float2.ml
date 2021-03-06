let rec dbl f = f +. f in
let (x, y) = 2.3, 4.5 in
print_float (dbl x *. 100.0);
print_newline ()
