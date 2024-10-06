% prime.pl

:- initialization(main).

is_prime(2).
is_prime(N) :- N > 2, \+ has_factor(N, 2).

has_factor(N, L) :- N mod L =:= 0.
has_factor(N, L) :- L * L < N, L2 is L + 1, has_factor(N, L2).

main :-
    write('Enter a number to check if it\'s prime: '),
    read(Number),
    (   is_prime(Number)
    ->  format('~w is prime.', [Number])
    ;   format('~w is not prime.', [Number])
    ),
    nl,
    halt.
