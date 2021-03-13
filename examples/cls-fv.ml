let x = 2 in
let rec f y = (x, y) in
let rec g z = f z in
print_int (let (x, y) = g 2 in x); ()
