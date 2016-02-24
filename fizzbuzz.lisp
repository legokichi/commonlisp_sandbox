(defun fizzbuzz-1 (n)
  (cond
    ((eq (mod n 15) 0) "fizzbuzz")
    ((eq (mod n 5) 0)  "buzz")
    ((eq (mod n 3) 0)  "fizz")
    (T n)))

(defun fizzbuzz ()
  (loop for i from 0 to 100 by 1 collect (fizzbuzz-1 i)))
		      
