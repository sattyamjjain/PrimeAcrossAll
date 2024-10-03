def is_prime(n):
    # Check for edge cases
    if n <= 1:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False

    # Check for factors from 3 to sqrt(n)
    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0:
            return False
    return True

# Example Usage
if __name__ == "__main__":
    number = int(input("Enter a number to check if it's prime: "))
    if is_prime(number):
        print(f"{number} is a prime number.")
    else:
        print(f"{number} is not a prime number.")
