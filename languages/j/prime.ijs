NB. Check if a number is prime in J
isPrime =: 3 : '1 = +/ 0 = n | i. 2 + >: %: n'
NB. Take the input from the user
NB. Command-line arguments in J can be accessed using ARGV_x

n =: ". ARGV_0  NB. Convert input to number
if. isPrime n do.
    'The number is prime.'
else.
    'The number is not prime.'
end.
