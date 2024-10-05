#!/bin/bash

# Check if a number is provided
if [ -z "$1" ]; then
    echo "No number provided."
    exit 1
fi

# Check if the input is a valid number
if ! [[ "$1" =~ ^[0-9]+$ ]]; then
    echo "Invalid input: not a number."
    exit 1
fi

# Check if the number is prime
num=$1
is_prime=1
for (( i=2; i*i<=num; i++ )); do
    if (( num % i == 0 )); then
        is_prime=0
        break
    fi
done

if (( is_prime )); then
    echo "$num is a prime number."
else
    echo "$num is not a prime number."
fi
