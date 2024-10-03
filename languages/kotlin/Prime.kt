import kotlin.math.sqrt

fun isPrime(n: Int): Boolean {
    if (n <= 1) return false
    if (n == 2) return true
    if (n % 2 == 0) return false

    for (i in 3..sqrt(n.toDouble()).toInt() step 2) {
        if (n % i == 0) return false
    }
    return true
}

fun main() {
    print("Enter a number to check if it's prime: ")
    val number = readLine()?.toIntOrNull()

    if (number != null) {
        if (isPrime(number)) {
            println("$number is a prime number.")
        } else {
            println("$number is not a prime number.")
        }
    } else {
        println("Invalid input.")
    }
}
