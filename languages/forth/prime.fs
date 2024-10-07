: is-prime ( n -- flag )
    dup 1 <= if drop false exit then
    dup 3 <= if drop true exit then
    dup 2 mod 0= if drop false exit then
    dup 3 mod 0= if drop false exit then
    5 swap begin
        dup dup * over <=
    while
        over over mod 0= if 2drop false exit then
        over 2 + over mod 0= if 2drop false exit then
        6 + swap
    repeat
    2drop true
;

: main
    cr ." Enter a number to check if it's prime: " cr
    pad dup 20 accept
    pad swap evaluate
    is-prime if
        ." It is a prime number." cr
    else
        ." It is not a prime number." cr
    then
;

main
