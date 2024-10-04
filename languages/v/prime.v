import math
import os

fn is_prime(n int) bool {
    if n <= 1 {
        return false
    }
    if n == 2 {
        return true
    }

    for i := 2; i <= math.sqrt(n); i++ {
        if n % i == 0 {
            return false
        }
    }
    return true
}

fn main() {
    print('Enter a number to check if it\'s prime: ')
    input := os.input_opt('') or { return }
    number := input.int()

    if is_prime(number) {
        println('$number is a prime number.')
    } else {
        println('$number is not a prime number.')
    }
}
