is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  if (n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  
  for (i in seq(3, floor(sqrt(n)), by=2)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

cat("Enter a number to check if it's prime: ")
number <- as.integer(readLines("stdin", n=1))

if (is_prime(number)) {
  cat(number, "is a prime number.\n")
} else {
  cat(number, "is not a prime number.\n")
}
