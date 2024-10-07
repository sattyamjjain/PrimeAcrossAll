! languages/fortran/prime.f90
program prime_checker
  implicit none
  integer :: n, i
  logical :: is_prime

  print *, 'Enter a number to check if it is prime:'
  read *, n

  is_prime = .true.

  if (n <= 1) then
    is_prime = .false.
  elseif (n == 2 .or. n == 3) then
    is_prime = .true.
  elseif (mod(n, 2) == 0 .or. mod(n, 3) == 0) then
    is_prime = .false.
  else
    do i = 5, int(sqrt(real(n))), 6
      if (mod(n, i) == 0 .or. mod(n, i + 2) == 0) then
        is_prime = .false.
        exit
      end if
    end do
  end if

  if (is_prime) then
    print *, n, 'is a prime number.'
  else
    print *, n, 'is not a prime number.'
  end if

end program prime_checker
