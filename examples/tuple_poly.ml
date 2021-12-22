let rec f x =
  let rec g y =
    let rec h z =
      x, y, z in
    h in
  g in

let (a, b, c) = ((f 2) 3) 4 in
print_int (a + b + c);
print_newline ();
(* below code causes panic *)
(*let (af, bf, cf) = ((f 2.4) 3.5) 4.6 in*)
(*print_float (af +. bf +. cf);*)
(*print_newline ();*)
()
