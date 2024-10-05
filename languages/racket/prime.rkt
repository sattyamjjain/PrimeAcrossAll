#lang racket

(define (is-prime? n)
  (define (check-divisor d)
    (cond
      [(= d 1) #t]
      [(= (modulo n d) 0) #f]
      [else (check-divisor (- d 1))]))
  (if (<= n 1)
      #f
      (check-divisor (sub1 n))))

(define (main)
  (display "Enter a number to check if it's prime: ")
  (flush-output)
  (define number (read))
  (if (is-prime? number)
      (printf "~a is a prime number.~n" number)
      (printf "~a is not a prime number.~n" number)))

(main)
