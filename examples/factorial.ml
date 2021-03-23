let rec fact n = if n = 0 then 1 else n * fact (n - 1) in
print_int (fact 5);
print_newline ();
print_int (fact 6);
print_newline ();
print_int (fact 7);
print_newline ();
()
