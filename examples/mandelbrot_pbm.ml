let rec dbl f = f +. f in
let rec yloop y =
  if y >= 1000 then () else
  let rec xloop x y =
    if x >= 1000 then () else
    let cr = dbl (float_of_int x) /. 1000.0 -. 1.5 in
    let ci = dbl (float_of_int y) /. 1000.0 -. 1.0 in
    let rec iloop i zr zi zr2 zi2 cr ci =
      if i = 0 then print_int 1 else
      let tr = zr2 -. zi2 +. cr in
      let ti = dbl zr *. zi +. ci in
      let zr = tr in
      let zi = ti in
      let zr2 = zr *. zr in
      let zi2 = zi *. zi in
      if zr2 +. zi2 > 2.0 *. 2.0 then print_int 0 else
      iloop (i - 1) zr zi zr2 zi2 cr ci in
    iloop 1000 0.0 0.0 0.0 0.0 cr ci;
    xloop (x + 1) y in
  xloop 0 y;
  print_newline ();
  yloop (y + 1) in
print_byte 80;
print_int 1;
print_byte 10;
print_int 1000;
print_byte 32;
print_int 1000;
print_byte 10;
yloop 0
