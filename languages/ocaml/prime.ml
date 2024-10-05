let is_prime n =
    let rec check_from d =
        d * d > n || (n mod d <> 0 && check_from (d + 1))
    in
    n > 1 && check_from 2

let () =
    print_endline "Enter a number:";
    let n = read_int () in
    if is_prime n then
        Printf.printf "%d is a prime number.\n" n
    else
        Printf.printf "%d is not a prime number.\n" n
