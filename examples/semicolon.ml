let rec f x = print_int x in let rec g _ = print_newline () in f 2; g (); f 3; g ()
