# Qiskit doesn't natively check primes, but we can simulate quantum gates for fun.
# This program checks if a number is prime in a classical manner using Python.

def is_prime(n):
    if n <= 1:
        return False
    for i in range(2, int(n ** 0.5) + 1):
        if n % i == 0:
            return False
    return True

def main():
    print("Enter a number to check if it's prime:")
    number = int(input())
    if is_prime(number):
        print(f"{number} is a prime number.")
    else:
        print(f"{number} is not a prime number.")

if __name__ == '__main__':
    main()
