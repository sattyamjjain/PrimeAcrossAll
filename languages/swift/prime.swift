import Foundation

func isPrime(_ n: Int) -> Bool {
    if n <= 1 {
        return false
    }
    if n == 2 {
        return true
    }
    if n % 2 == 0 {
        return false
    }

    let sqrtN = Int(Double(n).squareRoot())
    for i in stride(from: 3, through: sqrtN, by: 2) {
        if n % i == 0 {
            return false
        }
    }
    return true
}

print("Enter a number to check if it's prime:", terminator: " ")
if let input = readLine(), let number = Int(input) {
    if isPrime(number) {
        print("\(number) is a prime number.")
    } else {
        print("\(number) is not a prime number.")
    }
} else {
    print("Invalid input.")
}
