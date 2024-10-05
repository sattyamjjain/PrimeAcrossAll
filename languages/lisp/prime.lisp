(defun is-prime (n)
  (if (< n 2)
      nil
      (loop for i from 2 to (sqrt n) never (zerop (mod n i)))))

(defun main ()
  (format t "Enter a number: ")
  (let ((n (read)))
    (if (is-prime n)
        (format t "~d is a prime number.~%" n)
        (format t "~d is not a prime number.~%" n))))

(main)
