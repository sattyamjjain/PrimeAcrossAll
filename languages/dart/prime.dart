import 'dart:io';
import 'dart:math';

bool isPrime(int n) {
  if (n <= 1) return false;
  if (n == 2) return true;

  for (int i = 2; i <= sqrt(n); i++) {
    if (n % i == 0) return false;
  }
  return true;
}

void main() {
  stdout.write("Enter a number to check if it's prime: ");
  int? number = int.parse(stdin.readLineSync()!);

  if (isPrime(number)) {
    print("$number is a prime number.");
  } else {
    print("$number is not a prime number.");
  }
}
