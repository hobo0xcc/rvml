let rec f x =
    let rec g y =
        let rec h z =
            x + y + z in
        h in
    g in
((f 2) 3) 4
