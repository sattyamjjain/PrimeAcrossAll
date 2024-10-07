# prime.vy - Vyper contract to check if a number is prime

@public
@view
def is_prime(n: uint256) -> bool:
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0 or n % 3 == 0:
        return False

    max_i: uint256 = 1000  # Adjust as needed

    for i in range(uint256(5), max_i, uint256(6)):
        if i * i > n:
            break
        if n % i == 0 or n % (i + 2) == 0:
            return False
    return True
