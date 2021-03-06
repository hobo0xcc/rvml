let rec f x =
  let rec g y =
    let rec h z =
      x, y, z in
    h in
  g in

let (x, y, z) = ((f 2) 3) 4 in
print_int (x + y + z);
print_newline ();
let (xf, yf, zf) = ((f 2.4) 3.5) 4.6 in
print_float (xf +. yf +. zf);
print_newline ();
()
