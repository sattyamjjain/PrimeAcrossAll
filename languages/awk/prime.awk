#!/usr/bin/awk -f

# AWK script to check if a number is prime
{
    number = $1
    if (number < 2) {
        print number " is not a prime number."
        exit
    }

    for (i = 2; i <= sqrt(number); i++) {
        if (number % i == 0) {
            print number " is not a prime number."
            exit
        }
    }

    print number " is a prime number."
}
