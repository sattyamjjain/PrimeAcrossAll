% prime.pl - Prolog program to check if a number is prime

:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    ( Argv = [Arg] ->
        atom_number(Arg, Number)
    ; 
        write('Enter a number to check if it\'s prime: '),
        read(Number)
    ),
    ( is_prime(Number) ->
        format('~w is a prime number.~n', [Number])
    ;   format('~w is not a prime number.~n', [Number])
    ),
    halt.

is_prime(2).
is_prime(3).
is_prime(N) :-
    N > 3,
    N mod 2 =\= 0,
    N mod 3 =\= 0,
    \+ has_factor(N,5).

has_factor(N,L) :-
    L * L =< N,
    ( N mod L =:= 0 ;
      N mod (L + 2) =:= 0 ;
      L2 is L + 6,
      has_factor(N,L2)
    ).
