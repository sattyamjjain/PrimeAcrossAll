function isPrime(n) {
    if (n <= 1) return false;
    if (n <= 3) return true;
    if (n % 2 === 0 || n % 3 === 0) return false;
    for (var i = 5; i * i <= n; i += 6) {
      if (n % i === 0 || n % (i + 2) === 0) return false;
    }
    return true;
  }
  
  var number = parseInt(prompt("Enter a number to check if it's prime:"));
  if (isPrime(number)) {
    console.log(number + " is a prime number.");
  } else {
    console.log(number + " is not a prime number.");
  }
  