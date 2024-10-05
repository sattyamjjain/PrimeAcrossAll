\ languages/forth/prime.fs
: is-prime ( n -- flag )
    dup 1 <= if
        drop 0 exit
    then
    2 over < if
        drop 1 exit
    then
    2 swap / 2 swap
    do
        over i mod 0= if
            drop 0 exit
        then
    loop
    drop 1
;

: main
    cr ." Enter a number to check if it's prime: " 
    read
    dup is-prime if
        ." is a prime number." 
    else
        ." is not a prime number."
    then
    cr
;

main
