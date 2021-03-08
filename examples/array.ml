let a = Array.make 2 0 in
a.(0) <- 1;
a.(1) <- 2;
a.(2) <- 3;
print_int (a.(0) + a.(1) + a.(2));
print_newline ();
()
