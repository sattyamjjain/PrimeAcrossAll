% Prolog program to check if a number is prime

is_prime(2).
is_prime(N) :-
    N > 2,
    \+ has_factor(N, 2).

% Helper predicate to check if there's a factor
has_factor(N, F) :-
    N mod F =:= 0.
has_factor(N, F) :-
    F * F < N,
    F2 is F + 1,
    has_factor(N, F2).

% Main function to check prime
main :-
    write('Enter a number to check if it\'s prime: '),
    read(Number),
    (is_prime(Number) -> writeln(Number), writeln(' is a prime number.');
    writeln(Number), writeln(' is not a prime number.')).

:- initialization(main).
