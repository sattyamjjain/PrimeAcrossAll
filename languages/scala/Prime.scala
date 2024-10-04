object Prime {
  def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else !(2 to math.sqrt(n).toInt).exists(x => n % x == 0)
  }

  def main(args: Array[String]): Unit = {
    println("Enter a number to check if it's prime: ")
    val input = scala.io.StdIn.readInt()
    if (isPrime(input))
      println(s"$input is a prime number.")
    else
      println(s"$input is not a prime number.")
  }
}
